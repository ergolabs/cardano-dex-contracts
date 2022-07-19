{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}

module ErgoDex.Contracts.Typed where

import qualified Prelude as Haskell

import           Data.Aeson              (FromJSON, ToJSON)
import           ErgoDex.Contracts.Types
import           ErgoDex.Contracts.Class
import qualified ErgoDex.Contracts.Pool as P
import           PlutusTx.Prelude
import GHC.Generics (Generic)

data PoolConfig = PoolConfig
  { poolNft    :: Coin Nft
  , poolX      :: Coin X
  , poolY      :: Coin Y
  , poolLq     :: Coin Liquidity
  , poolFeeNum :: Integer
  } deriving (Haskell.Show, Haskell.Eq, Generic, ToJSON, FromJSON)

instance UnliftErased PoolConfig P.PoolConfig where
  lift PoolConfig{..} = P.PoolConfig
    { poolNft    = unCoin poolNft
    , poolX      = unCoin poolX
    , poolY      = unCoin poolY
    , poolLq     = unCoin poolLq
    , poolFeeNum = poolFeeNum
    }

  unlift P.PoolConfig{..} = PoolConfig
    { poolNft    = Coin poolNft
    , poolX      = Coin poolX
    , poolY      = Coin poolY
    , poolLq     = Coin poolLq
    , poolFeeNum = poolFeeNum
    }
