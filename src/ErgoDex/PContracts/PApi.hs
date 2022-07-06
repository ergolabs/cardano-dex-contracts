module ErgoDex.PContracts.PApi
 ( containsSignature
 , ownCurrencySymbol
 , ownStakingCredential
 , getRewardValue'
 , tletUnwrap
 , pmin
 , getInputValue
 , checkPoolNft
 , checkInputsQty
 , zero
 , zeroAsData
 , getPoolDatum
 , maxLqCap
 , burnLqInitial
 , feeDen
 ) where

import Plutarch
import Plutarch.Api.V1
import Plutarch.Prelude

import PExtra.List
import PExtra.Monadic (tlet, tletField, tmatch, tmatchField)
import PExtra.API

import qualified ErgoDex.Contracts.Pool as P

maxLqCap :: Term s PInteger
maxLqCap = pconstant P.maxLqCap

burnLqInitial :: Term s PInteger
burnLqInitial = pconstant P.burnLqInitial

feeDen :: Term s PInteger
feeDen = pconstant 1000

zero :: Term s PInteger
zero = pconstant 0

zeroAsData :: Term s (PAsData PInteger)
zeroAsData = pdata zero

tletUnwrap :: (PIsData a) => Term s (PAsData a) -> TermCont @r s (Term s a)
tletUnwrap = tlet . pfromData

pmin :: POrd a => Term s (a :--> a :--> a)
pmin = phoistAcyclic $ plam $ \a b -> pif (a #<= b) a b

containsSignature :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PPubKeyHash :--> PBool)
containsSignature = phoistAcyclic $ plam $ \signatories userPubKeyHash -> pelem # pdata userPubKeyHash # signatories

-- Guarantees correctness of reward proposition
getRewardValue' :: Term s (PAsData PTxOut :--> PPubKeyHash :--> PMaybeData PPubKeyHash :--> PValue)
getRewardValue' = phoistAcyclic $ plam $ \out pubkeyHash stakePkhM -> unTermCont $ do
  let addr  = pfield @"address" # out
  cred      <- tletField @"credential" addr
  outValue  <- tletUnwrap $ pmatch cred $ \case
    PPubKeyCredential pcred ->
      let
        pkh   = pfield @"_0" # pcred
        value = pfield @"value" # out
      in pif (pkh #== pubkeyHash) value (ptraceError "Invalid reward proposition")
    _ -> ptraceError "Invalid reward proposition"
  sPkh     <- tlet $ getStakeHash # addr
  pure $ pif (sPkh #== stakePkhM) outValue (ptraceError "Invalid reward proposition")

getStakeHash :: forall (s :: S). Term s (PAddress :--> PMaybeData PPubKeyHash)
getStakeHash = phoistAcyclic $ plam $ \address -> unTermCont $ do
  sCredM   <- tletField @"stakingCredential" address
  pure $ pmatch sCredM $ \case
    PDJust sCredData ->
      plet (pfield @"_0" # sCredData) $ \sCred ->
        pmatch sCred $ \case
          PStakingHash c ->
            plet (pfield @"_0" # c) $ \sHash ->
	            pmatch sHash $ \case
	              PPubKeyCredential pscred -> pcon (PDJust pscred)
	              _ -> ptraceError "Invalid stake proposition"
          _ -> ptraceError "Invalid stake proposition"
    _ -> pcon (PDNothing pdnil)

getInputValue :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PInteger :--> PValue)
getInputValue = phoistAcyclic $ plam $ \inputs index -> unTermCont $ do
  possiblePoolInput <- tletUnwrap (pelemAt # index # inputs)
  output            <- tletUnwrap $ pfield @"resolved" # possiblePoolInput
  pure $ pfield @"value" # output

checkPoolNft :: Term s (PValue :--> PAssetClass :--> PBool)
checkPoolNft = phoistAcyclic $ plam $ \value poolNft ->
  let nftQty = assetClassValueOf # value # poolNft
  in pif (nftQty #== 1)
    (pcon PTrue)
    (ptraceError "Invalid pool")

checkInputsQty :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PBool)
checkInputsQty = phoistAcyclic $ plam $ \inputs ->
  let inputsLength = plength # inputs
  in inputsLength #== 2

getPoolDatum :: Term s (PInteger :--> PTxInfo :--> PDatum)
getPoolDatum = phoistAcyclic $ plam $ \index txInfo -> unTermCont $ do
  datums     <- tletUnwrap $ pfield @"data" # txInfo
  maybeTuple <- tlet $ pfromData (pelemAt # index # datums)
  tletUnwrap $ pfield @"_1" # maybeTuple

ownCurrencySymbol :: Term s (PScriptContext :--> PCurrencySymbol)
ownCurrencySymbol = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    PScriptContext te <- tmatch sc
    PMinting cs'      <- tmatchField @"purpose" te
    pure $ pfield @"_0" # cs'

ownStakingCredential :: Term s (PScriptContext :--> PStakingCredential)
ownStakingCredential = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    PScriptContext te <- tmatch sc
    PRewarding cr'    <- tmatchField @"purpose" te
    pure $ pfield @"_0" # cr'
