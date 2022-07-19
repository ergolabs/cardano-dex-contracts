{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Plutus where

import qualified Plutus.V1.Ledger.Ada     as Ada
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Contexts
import           PlutusTx.Prelude

{-# INLINABLE adaAssetClass #-}
adaAssetClass :: AssetClass
adaAssetClass = assetClass Ada.adaSymbol Ada.adaToken

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE inputsNum #-}
inputsNum :: ScriptContext -> Integer
inputsNum sCtx = length $ txInfoInputs $ scriptContextTxInfo sCtx
