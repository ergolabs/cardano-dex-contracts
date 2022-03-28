{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}

module Models.Generator
  ( genTxOutRefPool
  , genTxOutRefOrder
  , genTxOutRef
  , genTokenName
  , genCurrencySymbol
  , genAssetClass
  , genValue
  , genValues
  , genAdaValue
  , genPoolConfig
  , genDepositConfig
  , genDatum
  , genDatumHash
  , genOrderDatum
  , genPoolRedeemer
  , genRedeemer
  , genMaxLq
  , genTxInType
  , genScriptCredential
  , pubKeyHashReward
  ) where

import Models.Utils

import qualified ErgoDex.Contracts.Pool as P
import qualified ErgoDex.Contracts.Proxy.Deposit  as Deposit
import qualified ErgoDex.Contracts.Proxy.Order as Order
import qualified ErgoDex.PValidators as PScripts

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value (AssetClass(..))
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import PlutusTx.AssocMap as Map
import qualified Ledger.Ada      as Ada
import qualified Ledger as Ledger
import qualified Ledger.Typed.Scripts.Validators as LV
import           Plutus.V1.Ledger.Credential (Credential (..))
import qualified PlutusTx
import qualified Ledger.Interval as Interval

import qualified Data.Set as Set
import qualified Data.ByteString         as BS
import GHC.Generics

genTokenName :: BS.ByteString -> TokenName
genTokenName = TokenName . BuiltinByteString

genCurrencySymbol :: BS.ByteString -> CurrencySymbol
genCurrencySymbol = CurrencySymbol . BuiltinByteString

genAssetClass :: CurrencySymbol -> TokenName -> AssetClass
genAssetClass cs tn = AssetClass (cs, tn)

genValue :: AssetClass -> Integer -> Value
genValue (AssetClass (cs, tn)) qty = Value $ Map.fromList [(cs, Map.singleton tn qty)]

genAdaValue :: Integer -> Value
genAdaValue qty = genValue (genAssetClass Ada.adaSymbol Ada.adaToken) qty

genValues :: [Value] -> Value -> Value
genValues (x:xs) acc = genValues xs (x <> acc)
genValues [] acc = acc

genPoolConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> P.PoolConfig
genPoolConfig nft x y lq fee = P.PoolConfig nft x y lq fee

genDepositConfig :: AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> PubKeyHash -> Integer -> Deposit.DepositConfig
genDepositConfig nft x y lq fee pkh cFee = Deposit.DepositConfig nft x y lq fee pkh cFee

genDatum :: P.PoolConfig -> Datum
genDatum = Datum . toBuiltinData

genOrderDatum :: Deposit.DepositConfig -> Datum
genOrderDatum = Datum . toBuiltinData

genDatumHash :: Datum -> DatumHash
genDatumHash datum = Ledger.datumHash datum

genPoolRedeemer :: Integer -> P.PoolAction -> P.PoolRedeemer
genPoolRedeemer ix action = P.PoolRedeemer action ix

genDepositRedeemer :: Integer -> Integer -> Integer -> Order.OrderRedeemer
genDepositRedeemer a b c = Order.OrderRedeemer a b c Order.Apply

genDepositRedeemerR :: Order.OrderRedeemer -> Redeemer
genDepositRedeemerR = Redeemer . toBuiltinData

genRedeemer :: P.PoolRedeemer -> Redeemer
genRedeemer = Redeemer . toBuiltinData

genMaxLq :: Integer
genMaxLq = 0x7fffffffffffffff

genTxInType :: Datum -> Redeemer -> Ledger.TxInType
genTxInType datum redeemer = Ledger.ConsumeScriptAddress PScripts.poolValidator redeemer datum 

genScriptCredential :: Credential
genScriptCredential = ScriptCredential $ LV.validatorHash $ LV.unsafeMkTypedValidator $ PScripts.poolValidator

pubKeyHashReward :: PubKeyHash
pubKeyHashReward = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"