module ErgoDex.PContracts.PStaking
  ( poolAdaStakingValidatorT
  ) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1
import PExtra.Monadic
import PExtra.API (PAssetClass)
import ErgoDex.PContracts.PApi  (ownStakingCredential, tletUnwrap)
import ErgoDex.PContracts.PPool (findPoolOutput)
import PExtra.List (pelemAt, pfind)
import PExtra.Ada (pGetLovelace)

poolAdaStakingValidatorT :: Term s PAssetClass -> Term s (PInteger :--> PScriptContext :--> PBool)
poolAdaStakingValidatorT poolNft = plam $ \poolInIx ctx' -> unTermCont $ do
  ctx     <- tcont $ pletFields @'["txInfo"] ctx'
  txInfo' <- tletUnwrap $ hrecField @"txInfo" ctx
  txInfo  <- tcont $ pletFields @'["inputs", "outputs", "wdrl"] txInfo'
  inputs  <- tletUnwrap $ hrecField @"inputs" txInfo
  outputs <- tletUnwrap $ hrecField @"outputs" txInfo
  wdrl    <- tletUnwrap $ hrecField @"wdrl" txInfo

  poolOut      <- tlet $ pfromData $ findPoolOutput # poolNft # outputs -- nft is preserved
  poolOutValue <- tletField @"value" poolOut
  poolIn'      <- tlet $ pelemAt # poolInIx # inputs
  poolIn       <- tcont $ pletFields @'["resolved"] poolIn'
  poolInValue  <-
    let pool = pfromData $ hrecField @"resolved" poolIn
    in tletField @"value" pool
  let
    poolAdaIn  = pGetLovelace # poolInValue
    poolAdaOut = pGetLovelace # poolOutValue
    ownWdrl =
      pfind
        # plam (\tuple ->
          let
            cred     = pfield @"_0" # tuple
            selfCred = pdata $ ownStakingCredential # ctx'
          in cred #== selfCred)
        # wdrl
    ownWdrAmount = pmatch ownWdrl $ \case
      PJust tp -> pfield @"_1" # tp
      PNothing -> ptraceError "Withdrawal not found" 
  -- todo: check that pool.redeemer == RewardWdrl
  pure $ ownWdrAmount #<= poolAdaOut - poolAdaIn -- withdrawal to pool
