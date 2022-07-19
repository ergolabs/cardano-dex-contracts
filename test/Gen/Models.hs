{-# LANGUAGE OverloadedStrings #-}

module Gen.Models
  ( genTokenName
  , genTxId
  , genTxOutRef
  , genCurrencySymbol
  , mkAdaAssetClass
  , genAssetClass
  , mkAssetClass
  , mkValue
  , mkAdaValue
  , mkValues
  , mkPoolConfig
  , mkDepositConfig
  , mkPoolRedeemer
  , mkDepositRedeemer
  , mkOrderRedeemer
  , mkRedeemer
  , mkDatum
  , mkDatumHash
  , mkMaxLq
  , mkTxInType
  , mkScriptCredential
  , genPkh
  , mkDepositValidator
  , mkSwapValidator
  , mkPoolValidator
  , mkTxOut
  , mkTxOut'
  , mkTxIn
  , mkTxInfo
  , mkPoolTxInfo
  , mkPurpose
  , mkContext
  ) where

import RIO

import Hedgehog
import Hedgehog.Gen   as Gen
import Hedgehog.Range as Range

import qualified Data.ByteString as BS

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value    as Value
import PlutusTx.Builtins.Internal
import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Api as Ledger
import Plutus.V1.Ledger.Contexts
import Plutarch.Api.V1 ( validatorHash )
import qualified Plutus.V1.Ledger.Interval as Interval

import qualified ErgoDex.PValidators             as PScripts
import qualified ErgoDex.Contracts.Pool          as P
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import qualified ErgoDex.Contracts.Proxy.Order   as O
import Plutus.V1.Ledger.Tx (TxInType (ConsumeScriptAddress))
import PlutusTx.Builtins as Builtins

genBuiltinByteString :: MonadGen f => Int -> f BuiltinByteString
genBuiltinByteString s = bytes (Range.singleton s) <&> BuiltinByteString

random32bs :: MonadGen f => f BuiltinByteString
random32bs = genBuiltinByteString 32

genTxId :: MonadGen f => f TxId
genTxId = prune $ random32bs <&> TxId

genTxOutRef :: MonadGen f => f TxOutRef
genTxOutRef = do
  txId <- genTxId
  ix   <- integral $ Range.constant 0 10
  pure $ TxOutRef txId ix

genTokenName :: MonadGen f => f TokenName
genTokenName = do
  bs <- random32bs
  return $ TokenName bs

genCurrencySymbol :: MonadGen f => f CurrencySymbol
genCurrencySymbol = do
  bs <- random32bs
  return $ CurrencySymbol bs

mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
mkAssetClass cs tn = AssetClass (cs, tn)

genAssetClass :: MonadGen f => f AssetClass
genAssetClass = do
  tn <- genTokenName
  cs <- genCurrencySymbol
  return $ AssetClass (cs, tn)

mkAdaAssetClass :: AssetClass
mkAdaAssetClass = mkAssetClass Ada.adaSymbol Ada.adaToken

mkValue :: AssetClass -> Integer -> Value
mkValue (AssetClass (cs, tn)) qty = Value.singleton cs tn qty

mkAdaValue :: Integer -> Value
mkAdaValue qty = mkValue mkAdaAssetClass qty

mkValues :: [Value] -> Value -> Value
mkValues (x:xs) acc = mkValues xs (x <> acc)
mkValues [] acc = acc

mkPoolConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> P.PoolConfig
mkPoolConfig nft x y lq fee = P.PoolConfig nft x y lq fee

mkDepositConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> Integer -> D.DepositConfig
mkDepositConfig nft x y lq fee pkh cFee = D.DepositConfig nft x y lq fee pkh Nothing cFee

mkPoolRedeemer :: Integer -> P.PoolAction -> P.PoolRedeemer
mkPoolRedeemer ix action = P.PoolRedeemer action ix

mkDepositRedeemer :: Integer -> Integer -> Integer -> O.OrderRedeemer
mkDepositRedeemer a b c = O.OrderRedeemer a b c O.Apply

mkOrderRedeemer :: Integer -> Integer -> Integer -> O.OrderRedeemer
mkOrderRedeemer a b c = O.OrderRedeemer a b c O.Apply

mkRedeemer :: ToData a => a -> Redeemer
mkRedeemer = Redeemer . toBuiltinData

mkDatum :: ToData a => a -> Datum
mkDatum = Datum . toBuiltinData

dataHash :: Builtins.BuiltinData -> Builtins.BuiltinByteString
dataHash = undefined 

mkDatumHash :: Datum -> DatumHash
mkDatumHash = DatumHash . dataHash . getDatum

mkMaxLq :: Integer
mkMaxLq = 0x7fffffffffffffff

mkTxInType :: Datum -> Redeemer -> TxInType
mkTxInType datum redeemer = ConsumeScriptAddress PScripts.poolValidator redeemer datum 

mkScriptCredential :: Credential
mkScriptCredential = ScriptCredential $ validatorHash PScripts.poolValidator

genPkh :: MonadGen f => f PubKeyHash
genPkh = genBuiltinByteString 28 <&> PubKeyHash

mkDepositValidator :: ValidatorHash
mkDepositValidator = validatorHash PScripts.depositValidator

mkPoolValidator :: ValidatorHash
mkPoolValidator = validatorHash PScripts.poolValidator

mkSwapValidator :: ValidatorHash
mkSwapValidator = validatorHash PScripts.swapValidator

mkTxOut :: DatumHash -> Value -> ValidatorHash -> TxOut
mkTxOut dh v vh =
  TxOut
    { txOutAddress   = Address (ScriptCredential vh) Nothing
    , txOutValue     = v
    , txOutDatumHash = Just dh
    }

mkTxOut' :: DatumHash -> Value -> PubKeyHash -> TxOut
mkTxOut' dh v pkh =
  TxOut
    { txOutAddress   = Address (PubKeyCredential pkh) Nothing
    , txOutValue     = v
    , txOutDatumHash = Just dh
    }

mkTxIn :: TxOutRef -> TxOut -> TxInInfo
mkTxIn ref out =
  TxInInfo
    { txInInfoOutRef   = ref
    , txInInfoResolved = out
    }

mkPoolTxInfo :: TxInInfo -> TxOut -> TxInfo
mkPoolTxInfo pIn pOut =
  TxInfo
    { txInfoInputs = [pIn]
    , txInfoOutputs = [pOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoData = []
    , txInfoId = "b0"
    }


mkTxInfo :: TxInInfo -> TxInInfo -> TxOut -> TxOut -> TxInfo
mkTxInfo pIn oIn pOut oOut =
  TxInfo
    { txInfoInputs = [pIn, oIn]
    , txInfoOutputs = [pOut, oOut]
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = mempty
    , txInfoData = []
    , txInfoId = "b0"
    }

mkPurpose :: TxOutRef -> ScriptPurpose
mkPurpose = Spending

mkContext :: TxInfo -> ScriptPurpose -> ScriptContext
mkContext cxt purpose = ScriptContext cxt purpose