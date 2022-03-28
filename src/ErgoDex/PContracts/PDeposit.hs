{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErgoDex.PContracts.PDeposit
  ( DepositConfig(..)
  , depositValidatorT
  ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.Prelude
import Plutarch.DataRepr
import Plutarch.Api.V1.Contexts
import Plutarch.Lift

import PExtra.API
import PExtra.Ada
import Plutarch.Api.V1 (PPubKeyHash, PValue)
import PExtra.Monadic  (tlet, tmatch, tletField)
import PExtra.List     (pelemAt)
import Plutarch.Lift

import ErgoDex.PContracts.PApi   (getRewardValue', maxLqCap, pmin, tletUnwrap, containsSignature)
import ErgoDex.PContracts.POrder (OrderRedeemer, OrderAction(Refund, Apply))
import qualified ErgoDex.Contracts.Proxy.Deposit as D

import qualified ErgoDex.Contracts.Proxy.Deposit as D

newtype DepositConfig (s :: S) = DepositConfig
  (
    Term s (
      PDataRecord
      '[ "poolNft"       ':= PAssetClass
       , "x"             ':= PAssetClass
       , "y"             ':= PAssetClass
       , "lq"            ':= PAssetClass
       , "exFee"         ':= PInteger
       , "rewardPkh"     ':= PPubKeyHash
       , "collateralAda" ':= PInteger
       ]
    )
  )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via (PIsDataReprInstances DepositConfig)

instance PUnsafeLiftDecl DepositConfig where type PLifted DepositConfig = D.DepositConfig
deriving via (DerivePConstantViaData D.DepositConfig DepositConfig) instance (PConstant D.DepositConfig)

depositValidatorT :: ClosedTerm (DepositConfig :--> OrderRedeemer :--> PScriptContext :--> PBool)
depositValidatorT = plam $ \conf' redeemer' ctx' -> unTermCont $ do
  ctx     <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  conf    <- tcont $ pletFields @'["x", "y", "lq", "poolNft", "exFee", "rewardPkh", "collateralAda"] conf'
  txInfo' <- tletUnwrap $ hrecField @"txInfo" ctx
  txInfo  <- tcont $ pletFields @'["inputs", "outputs", "signatories"] txInfo'
  inputs  <- tletUnwrap $ hrecField @"inputs" txInfo
  outputs <- tletUnwrap $ hrecField @"outputs" txInfo

  redeemer    <- tcont $ pletFields @'["poolInIx", "orderInIx", "rewardOutIx", "action"] redeemer'
  poolInIx    <- tletUnwrap $ hrecField @"poolInIx" redeemer
  orderInIx   <- tletUnwrap $ hrecField @"orderInIx" redeemer
  rewardOutIx <- tletUnwrap $ hrecField @"rewardOutIx" redeemer

  rewardOut   <- tlet $ pelemAt # rewardOutIx # outputs
  rewardPkh   <- tletUnwrap $ hrecField @"rewardPkh" conf
  rewardValue <- tlet $ getRewardValue' # rewardOut # rewardPkh

  poolIn'   <- tlet $ pelemAt # poolInIx # inputs
  poolIn    <- tcont $ pletFields @'["outRef", "resolved"] poolIn'
  poolValue <-
    let pool = pfromData $ hrecField @"resolved" poolIn
    in tletField @"value" pool
  let
    poolIdentity =
      let
        requiredNft = pfromData $ hrecField @"poolNft" conf
        nftAmount   = assetClassValueOf # poolValue # requiredNft
      in nftAmount #== 1

  selfIn'   <- tlet $ pelemAt # orderInIx # inputs
  selfIn    <- tcont $ pletFields @'["outRef", "resolved"] selfIn'
  selfValue <-
    let self = pfromData $ hrecField @"resolved" selfIn
    in tletField @"value" self

  PSpending selfRef' <- tmatch (pfromData $ hrecField @"purpose" ctx)
  let
    selfIdentity =
      let
        selfRef   = pfromData $ pfield @"_0" # selfRef'
        selfInRef = pfromData $ hrecField @"outRef" selfIn
      in selfRef #== selfInRef

  x  <- tletUnwrap $ hrecField @"x" conf
  y  <- tletUnwrap $ hrecField @"y" conf
  lq <- tletUnwrap $ hrecField @"lq" conf

  exFee         <- tletUnwrap $ hrecField @"exFee" conf
  collateralAda <- tletUnwrap $ hrecField @"collateralAda" conf

  let
    strictInputs =
      let inputsLength = plength # inputs
      in inputsLength #== 2

  liquidity <-
    let lqNegative = assetClassValueOf # poolValue # lq
    in tlet $ maxLqCap - lqNegative

  reservesX    <- tlet $ assetClassValueOf # poolValue # x
  reservesY    <- tlet $ assetClassValueOf # poolValue # y
  minRewardByX <- tlet $ minAssetReward # selfValue # x # reservesX # liquidity # exFee # collateralAda
  minRewardByY <- tlet $ minAssetReward # selfValue # y # reservesY # liquidity # exFee # collateralAda
  let
    actualReward = assetClassValueOf # rewardValue # lq
    validChange =
      pif (minRewardByX #== minRewardByY)
        (pcon PTrue)
        (pif (minRewardByX #< minRewardByY)
          (validChange' # rewardValue # y # minRewardByY # minRewardByX # reservesY # liquidity)
          (validChange' # rewardValue # x # minRewardByX # minRewardByY # reservesX # liquidity))
    minReward   = pmin # minRewardByX # minRewardByY
    validReward = minReward #<= actualReward
--      let actualReward = assetClassValueOf # rewardValue # lq
--      in minReward #<= actualReward
    actualRewardIs0 = actualReward #== 0
    minRewardMoreThan10 = 10 #< minReward
  action <- tletUnwrap $ hrecField @"action" redeemer
  _ <- tlet $ ptraceIfTrue  ("actualRewardIs0") actualRewardIs0
  _ <- tlet $ ptraceIfTrue  ("minRewardMoreThan10") minRewardMoreThan10
  _ <- tlet $ ptraceIfFalse "poolIdentity" poolIdentity
  _ <- tlet $ ptraceIfFalse "selfIdentity" selfIdentity
  _ <- tlet $ ptraceIfFalse "strictInputs" strictInputs
  _ <- tlet $ ptraceIfFalse "validChange" validChange
  _ <- tlet $ ptraceIfFalse "validReward" validReward
  pure $ pmatch action $ \case
    Apply  -> poolIdentity #&& selfIdentity #&& strictInputs #&& validChange #&& validReward
    Refund -> let sigs = pfromData $ hrecField @"signatories" txInfo
              in containsSignature # sigs # rewardPkh

-- Checks whether an asset overflow is returned back to user
validChange' :: Term s (PValue :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
validChange' =
  phoistAcyclic $
    plam $ \rewardValue overflowAsset overflowAssetInput otherAssetInput overflowAssetReserves liquidity ->
      let
        diff   = overflowAssetInput - otherAssetInput
        excess = pdiv # (diff * overflowAssetReserves) # liquidity
        change = assetClassValueOf # rewardValue # overflowAsset
      in excess #<= change

minAssetReward :: Term s (PValue :--> PAssetClass :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger)
minAssetReward =
  phoistAcyclic $
    plam $ \selfValue asset assetReserves liquidity exFee collateralAda ->
      unTermCont $ do
        assetInput <- tlet $ assetClassValueOf # selfValue # asset
        let depositInput = pif (pIsAda # asset) (assetInput - exFee - collateralAda) assetInput
        pure $ pdiv # (depositInput * liquidity) # assetReserves