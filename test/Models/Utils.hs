module Models.Utils
  ( genNft
  , genNftWrong
  , genX
  , genY
  , genLQ
  , genCS
  , genTxOutRefPool
  , genTxOutRefOrder
  , genTxOutRef
  , genIncorrectToken
  , mkTokenNameHex
  , initBSGenerator
  , BSGenerator(..)
  , randString
  , eraseRight
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import qualified Data.Text as T

import PlutusTx.Builtins.Internal (BuiltinByteString(..))

import qualified System.Random as Random
import System.IO.Unsafe
import Control.Monad

import Plutus.V1.Ledger.Api

randString :: IO String
randString = liftM (take 10 . Random.randomRs ('a','z')) Random.newStdGen

data BSGenerator = BSGenerator
  { genByteString :: IO BS.ByteString
  }

genIncorrectToken :: BS.ByteString
genIncorrectToken = mkByteString $ T.pack "4e46545f546f6b656e5f6e65775f706f6f6c0a4e4e4e" 

genNftWrong :: BS.ByteString
genNftWrong = mkByteString $ T.pack "4e46545f546f6b656e5f6e65775f706f6f6c0a4e4e4e"

genNft :: BS.ByteString
genNft = mkByteString $ T.pack "4e46545f546f6b656e5f6e65775f706f6f6c0a"

genX :: BS.ByteString
genX = mkByteString $ T.pack "415f546f6b656e5f6e65775f706f6f6c0a"

genY :: BS.ByteString
genY = mkByteString $ T.pack "425f546f6b656e5f6e65775f706f6f6c0a"

genLQ :: BS.ByteString
genLQ = mkByteString $ T.pack "6572676f6c6162736c70746f6b656e"

genCS :: BS.ByteString
genCS = mkByteString $ T.pack "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

genTxOutRef :: TxOutRef
genTxOutRef = TxOutRef (TxId (BuiltinByteString $ mkByteString $ T.pack "61b8766a3411be29c14c12e8ebc90287eaa8f75cb3fedf6f84674d554e1b0329")) 2

genTxOutRefPool :: TxOutRef
genTxOutRefPool = TxOutRef (TxId (BuiltinByteString $ mkByteString $ T.pack "61b8766a3411be29c14c12e8ebc90287eaa8f75cb3fedf6f84674d554e1b0329")) 0

genTxOutRefOrder :: TxOutRef
genTxOutRefOrder = TxOutRef (TxId (BuiltinByteString $ mkByteString $ T.pack "61b8766a3411be29c14c12e8ebc90287eaa8f75cb3fedf6f84674d554e1b0329")) 1

initBSGenerator :: BSGenerator
initBSGenerator = BSGenerator genByteString'

genRandomString :: Int -> IO String
genRandomString length = liftM (take length . Random.randomRs ('a','z')) Random.getStdGen

genRandomText :: Int -> IO T.Text
genRandomText length = fmap T.pack (genRandomString length)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

genByteString' :: IO BS.ByteString
genByteString' = fmap E.encodeUtf8 (genRandomText (fromIntegral 32))

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decode . E.encodeUtf8 $ input)

mkTokenNameHex :: T.Text -> TokenName
mkTokenNameHex input = TokenName $ unsafeFromEither $ fmap toBuiltin ((Hex.decode . E.encodeUtf8) $ input)

eraseRight :: Either a b -> Either a ()
eraseRight (Right _) = Right ()
eraseRight (Left l)  = Left l