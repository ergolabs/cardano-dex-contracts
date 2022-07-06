{-# LANGUAGE UndecidableInstances #-}

module ErgoDex.PContracts.PPool
  ( PoolConfig(..)
  , PoolAction(..)
  , PoolRedeemer(..)
  , poolValidatorT
  , findPoolOutput
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr        (PDataFields, DerivePConstantViaData(..), PIsDataReprInstances(..))
import Plutarch.Lift            (PUnsafeLiftDecl(..))
import Plutarch.Api.V1.Contexts (PScriptContext, PScriptPurpose(PSpending))
import Plutarch.Api.V1          (PTxOut, PMaybeData(PDJust))

import PExtra.Monadic (tcon, tlet, tletField, tmatch)
import PExtra.List    (pelemAt)
import PExtra.API     (assetClassValueOf, PAssetClass)

import qualified ErgoDex.Contracts.Pool as P

import Plutarch.Builtin (pforgetData, pasInt)
import Plutarch.Unsafe  (punsafeCoerce)

import ErgoDex.PContracts.PApi (burnLqInitial, feeDen, maxLqCap, tletUnwrap, zero)

newtype PoolConfig (s :: S) = PoolConfig
  (
    Term s (
      PDataRecord
        '[ "poolNft" ':= PAssetClass
         , "poolX"   ':= PAssetClass
         , "poolY"   ':= PAssetClass
         , "poolLq"  ':= PAssetClass
         , "feeNum"  ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PMatch, PIsData, PDataFields)
    via PIsDataReprInstances PoolConfig

instance PUnsafeLiftDecl PoolConfig where type PLifted PoolConfig = P.PoolConfig
deriving via (DerivePConstantViaData P.PoolConfig PoolConfig) instance (PConstant P.PoolConfig)

data PoolAction (s :: S) = Deposit | Redeem | Swap | RewardWdrl | Destroy

instance PIsData PoolAction where
  pfromData tx =
    let x = pasInt # pforgetData tx
    in pmatch' x pcon
  pdata x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType PoolAction where
  type PInner PoolAction _ = PInteger

  pcon' Deposit    = 0
  pcon' Redeem     = 1
  pcon' Swap       = 2
  pcon' RewardWdrl = 3
  pcon' Destroy    = 4

  pmatch' x f =
    pif (x #== 0) (f Deposit)
      (pif (x #== 1) (f Redeem)
        (pif (x #== 2) (f Swap)
          (pif (x #== 3) (f RewardWdrl) (f Destroy))))

newtype PoolRedeemer (s :: S) = PoolRedeemer
  (
    Term s (
      PDataRecord
        '[ "action" ':= PoolAction
         , "selfIx" ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PoolRedeemer

newtype PoolState (s :: S) = PoolState
  (
    Term s (
      PDataRecord
        '[ "reservesX" ':= PInteger
         , "reservesY" ':= PInteger
         , "liquidity" ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PoolState

newtype PoolDiff (s :: S) = PoolDiff
  (
    Term s (
      PDataRecord
        '[ "diffX"  ':= PInteger
         , "diffY"  ':= PInteger
         , "diffLq" ':= PInteger
         ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PoolDiff

readPoolState :: Term s (PoolConfig :--> PTxOut :--> PoolState)
readPoolState = phoistAcyclic $ plam $ \conf' out -> unTermCont $ do
  value  <- tletField @"value" out
  poolX  <- tletField @"poolX" conf'
  poolY  <- tletField @"poolY" conf'
  poolLq <- tletField @"poolLq" conf'
  let
    x     = pdata $ assetClassValueOf # value # poolX
    y     = pdata $ assetClassValueOf # value # poolY
    negLq = assetClassValueOf # value # poolLq
    lq    = pdata $ maxLqCap - negLq
  tcon $ PoolState
     $ pdcons @"reservesX" @PInteger # x
    #$ pdcons @"reservesY" @PInteger # y
    #$ pdcons @"liquidity" @PInteger # lq
     # pdnil

validDepositRedeem :: Term s (PoolState :--> PInteger :--> PInteger :--> PInteger :--> PBool)
validDepositRedeem = phoistAcyclic $ plam $ \state' dx dy dlq -> unTermCont $ do
  state <- tcont $ pletFields @'["reservesX", "reservesY", "liquidity"] state'
  rx    <- tletUnwrap $ hrecField @"reservesX" state
  ry    <- tletUnwrap $ hrecField @"reservesY" state
  lq    <- tletUnwrap $ hrecField @"liquidity" state
  pure $ dlq * rx #<= dx * lq #&& dlq * ry #<= dy * lq

validSwap :: Term s (PoolConfig :--> PoolState :--> PInteger :--> PInteger :--> PBool)
validSwap = phoistAcyclic $ plam $ \conf state' dx dy -> unTermCont $ do
  state   <- tcont $ pletFields @'["reservesX", "reservesY"] state'
  rx      <- tletUnwrap $ hrecField @"reservesX" state
  ry      <- tletUnwrap $ hrecField @"reservesY" state
  feeNum  <- tletField @"feeNum" conf
  feeDen' <- tlet feeDen

  dxf <- tlet $ dx * feeNum
  dyf <- tlet $ dy * feeNum
  pure $ pif (zero #< dx)
    (-dy * (rx * feeDen' + dxf) #<= ry * dxf)
    (-dx * (ry * feeDen' + dyf) #<= rx * dyf)

-- Guarantees preservation of pool NFT
findPoolOutput :: Term s (PAssetClass :--> PBuiltinList (PAsData PTxOut) :--> PAsData PTxOut)
findPoolOutput =
  phoistAcyclic $
    plam $ \nft ->
      precList
        (\self x xs ->
          let
            value = pfield @"value" # x
            amt   = assetClassValueOf # value # nft
          in pif (amt #== 1) x (self # xs))
        (const $ ptraceError "Pool output not found")

poolValidatorT :: ClosedTerm (PoolConfig :--> PoolRedeemer :--> PScriptContext :--> PBool)
poolValidatorT = plam $ \conf redeemer' ctx' -> unTermCont $ do
  redeemer <- tcont $ pletFields @'["action", "selfIx"] redeemer'
  selfIx   <- tletUnwrap $ hrecField @"selfIx" redeemer
  ctx      <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  txinfo'  <- tletUnwrap $ hrecField @"txInfo" ctx
  txInfo   <- tcont $ pletFields @'["inputs", "outputs"] txinfo'
  inputs   <- tletUnwrap $ hrecField @"inputs" txInfo
  selfIn'  <- tlet $ pelemAt # selfIx # inputs
  selfIn   <- tcont $ pletFields @'["outRef", "resolved"] selfIn'
  self     <- tletUnwrap $ hrecField @"resolved" selfIn

  s0  <- tlet $ readPoolState # conf # self
  lq0 <- tletField @"liquidity" s0

  action <- tletUnwrap $ hrecField @"action" redeemer

  pure $ pmatch action $ \case
    Destroy -> lq0 #<= burnLqInitial -- all tokens except for permanetly locked ones are removed
    _       -> unTermCont $ do
      outputs   <- tletUnwrap $ hrecField @"outputs" txInfo
      nft       <- tletField @"poolNft" conf
      successor <- tlet $ pfromData $ findPoolOutput # nft # outputs -- nft is preserved

      s1  <- tlet $ readPoolState # conf # successor
      rx0 <- tletField @"reservesX" s0
      rx1 <- tletField @"reservesX" s1
      ry0 <- tletField @"reservesY" s0
      ry1 <- tletField @"reservesY" s1
      lq1 <- tletField @"liquidity" s1
      let
        dx  = rx1 - rx0
        dy  = ry1 - ry0
        dlq = lq1 - lq0 -- pool keeps only the negative part of LQ tokens
      PSpending selfRef' <- tmatch (pfromData $ hrecField @"purpose" ctx)
      selfRef            <- tletField @"_0" selfRef'
      selfInRef          <- tletUnwrap $ hrecField @"outRef" selfIn
      let selfIdentity = selfRef #== selfInRef -- self is the output currently validated by this script 

      maybeSelfDh    <- tletField @"datumHash" self
      maybeSuccDh    <- tletField @"datumHash" successor
      PDJust selfDh' <- tmatch maybeSelfDh
      PDJust succDh' <- tmatch maybeSuccDh
      selfDh         <- tletField @"_0" selfDh'
      succDh         <- tletField @"_0" succDh'
      let confPreserved = succDh #== selfDh -- config preserved

      selfAddr <- tletField @"address" self
      succAddr <- tletField @"address" successor
      let scriptPreserved = succAddr #== selfAddr -- validator, staking cred preserved

      let
        validAction = pmatch action $ \case
          Swap       -> dlq #== 0 #&& validSwap # conf # s0 # dx # dy
          RewardWdrl -> dlq #== 0 #&& 0 #<= dx #&& 0 #<= dy
          _          -> validDepositRedeem # s0 # dx # dy # dlq

      pure $ selfIdentity #&& confPreserved #&& scriptPreserved #&& validAction
