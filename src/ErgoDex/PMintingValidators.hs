module ErgoDex.PMintingValidators
  ( poolNftMiningValidator
  , poolLqMiningValidator
  , wrapMintingValidator
  ) where

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1          (mkMintingPolicy)
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutarch.Unsafe          (punsafeCoerce)

import qualified ErgoDex.PContracts.PAssets as A
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Api (TokenName, MintingPolicy)

wrapMintingValidator
  :: PIsData rdmr
  => ClosedTerm (rdmr :--> PScriptContext :--> PBool)
  -> ClosedTerm (PData :--> PScriptContext :--> POpaque)
wrapMintingValidator validator = plam $ \rdmr' ctx ->
  let
    rdmr   = pfromData $ punsafeCoerce rdmr'
    result = validator # rdmr # ctx
  in popaque $ pif result (pcon PUnit) (ptraceError "Minting validator reduced to False")

poolNftMiningValidator :: TxOutRef -> TokenName -> MintingPolicy
poolNftMiningValidator oref tn =
  mkMintingPolicy
    $ wrapMintingValidator
      $ A.poolNftMintValidatorT (pconstant oref) (pconstant tn)

poolLqMiningValidator :: TxOutRef -> TokenName -> Integer -> MintingPolicy
poolLqMiningValidator oref tn emission =
  mkMintingPolicy
    $ wrapMintingValidator
      $ A.poolLqMintValidatorT (pconstant oref) (pconstant tn) (pconstant emission)
