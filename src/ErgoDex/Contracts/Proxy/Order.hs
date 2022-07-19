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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Proxy.Order
  ( OrderRedeemer(..)
  , OrderAction(..)
  , isAda
  ) where

import qualified Prelude as Haskell

import           Plutus.V1.Ledger.Value
import           PlutusTx.Prelude
import           PlutusTx.Builtins
import           PlutusTx.IsData.Class
import qualified PlutusTx

import ErgoDex.Plutus (adaAssetClass)

data OrderAction = Apply | Refund
  deriving Haskell.Show
PlutusTx.makeLift ''OrderAction

instance FromData OrderAction where
  {-# INLINE fromBuiltinData #-}
  fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
    where
      chooseAction i
        | i == 0    = Just Apply
        | i == 1    = Just Refund
        | otherwise = Nothing

instance UnsafeFromData OrderAction where
  {-# INLINE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = maybe (error ()) id . fromBuiltinData

instance ToData OrderAction where
  {-# INLINE toBuiltinData #-}
  toBuiltinData a = mkI $ case a of
    Apply  -> 0
    Refund -> 1

data OrderRedeemer = OrderRedeemer
  { poolInIx    :: Integer
  , orderInIx   :: Integer
  , rewardOutIx :: Integer
  , action      :: OrderAction
  } deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''OrderRedeemer [('OrderRedeemer, 0)]
PlutusTx.makeLift ''OrderRedeemer

{-# INLINABLE isAda #-}
isAda :: AssetClass -> Bool
isAda cls = cls == adaAssetClass