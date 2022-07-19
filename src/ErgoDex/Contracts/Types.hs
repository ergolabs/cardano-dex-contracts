{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Types where

import GHC.Generics (Generic)
import Data.Aeson   (FromJSON, ToJSON)

import           Plutus.V1.Ledger.Value        (AssetClass (..), assetClassValue, assetClassValueOf, Value(..))
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)
import           ErgoDex.Plutus      (adaAssetClass)

data Lovelace = Lovelace deriving (Haskell.Show, Haskell.Eq, Generic)

-- Pool NFT
data Nft = Nft deriving (Haskell.Show, Haskell.Eq, Generic)

-- First asset of a pool
data X = X deriving (Haskell.Show, Haskell.Eq, Generic)

-- Second asset of a pool
data Y = Y deriving (Haskell.Show, Haskell.Eq, Generic)

-- Liquidity token of a pool
data Liquidity = Liquidity deriving (Haskell.Show, Haskell.Eq, Generic)

-- First asset of a pool
data Quote = Quote deriving (Haskell.Show, Haskell.Eq, Generic)

-- Second asset of a pool
data Base = Base deriving (Haskell.Show, Haskell.Eq, Generic)

-- Type to distinguish tokens within a pool
newtype Coin a = Coin { unCoin :: AssetClass }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, Eq, Haskell.Eq, Haskell.Ord)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{-# INLINABLE retagCoin #-}
retagCoin :: forall a b . Coin a -> Coin b
retagCoin (Coin ac) = Coin ac

{-# INLINABLE valueOf #-}
valueOf :: Value -> Coin a -> Integer
valueOf v = assetClassValueOf v . unCoin

-- Difference of a token amount
newtype Diff a = Diff { unDiff :: Integer }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

-- Amount of a token
newtype Amount a = Amount { unAmount :: Integer }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{-# INLINABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINABLE isUnit #-}
isUnit :: Value -> Coin a -> Bool
isUnit v c = amountOf v c == 1

{-# INLINABLE coinAmountValue #-}
coinAmountValue :: Coin a -> Amount a -> Value
coinAmountValue (Coin ac) (Amount v) = assetClassValue ac v

{-# INLINABLE isAda #-}
isAda :: Coin a -> Bool
isAda (Coin cls) = cls == adaAssetClass
