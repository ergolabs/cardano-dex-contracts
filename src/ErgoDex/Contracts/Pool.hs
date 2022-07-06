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
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module ErgoDex.Contracts.Pool
  ( PoolConfig(..)
  , PoolAction(..)
  , PoolRedeemer(..)
  , PoolState(..)
  , maxLqCap
  , maxLqCapAmount
  , burnLqInitial
  , getPoolInput
  , findPoolConfig
  , readPoolState
  , mkPoolValidator
  ) where

import qualified Prelude as Haskell

import           Ledger
import           Ledger.Value            (flattenValue, assetClassValueOf)
import           Playground.Contract     (FromJSON, Generic, ToJSON, ToSchema)
import           ErgoDex.Contracts.Types
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx.IsData.Class
import           PlutusTx.Builtins

-- Unwrapped representation of PoolConfig
data PoolConfig = PoolConfig
  { poolNft    :: AssetClass
  , poolX      :: AssetClass
  , poolY      :: AssetClass
  , poolLq     :: AssetClass
  , poolFeeNum :: Integer
  } deriving (Haskell.Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''PoolConfig [('PoolConfig, 0)]
PlutusTx.makeLift ''PoolConfig

data PoolAction = Deposit | Redeem | Swap | RewardWdrl | Destroy
  deriving Haskell.Show
PlutusTx.makeLift ''PoolAction

instance FromData PoolAction where
  {-# INLINE fromBuiltinData #-}
  fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
    where
      chooseAction i
        | i == 0    = Just Deposit
        | i == 1    = Just Redeem
        | i == 2    = Just Swap
        | i == 3    = Just RewardWdrl
        | i == 4    = Just Destroy
        | otherwise = Nothing

instance UnsafeFromData PoolAction where
  {-# INLINE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = maybe (error ()) id . fromBuiltinData

instance ToData PoolAction where
  {-# INLINE toBuiltinData #-}
  toBuiltinData a = mkI $ case a of
    Deposit    -> 0
    Redeem     -> 1
    Swap       -> 2
    RewardWdrl -> 3
    Destroy    -> 4

data PoolRedeemer = PoolRedeemer
  { action :: PoolAction
  , selfIx :: Integer
  }
 deriving (Haskell.Show, Eq, Generic)
PlutusTx.makeIsDataIndexed ''PoolRedeemer [('PoolRedeemer, 0)]
PlutusTx.makeLift ''PoolRedeemer

data PoolState = PoolState
  { reservesX :: Integer
  , reservesY :: Integer
  , liquidity :: Integer
  } deriving Haskell.Show

{-# INLINABLE maxLqCap #-}
maxLqCap :: Integer
maxLqCap = 0x7fffffffffffffff

{-# INLINABLE burnLqInitial #-}
burnLqInitial :: Integer
burnLqInitial = 1000

maxLqCapAmount :: Amount Liquidity
maxLqCapAmount = Amount maxLqCap

{-# INLINABLE readPoolState #-}
readPoolState :: PoolConfig -> TxOut -> PoolState
readPoolState PoolConfig{..} out =
    PoolState x y lq
  where
    value = txOutValue out
    x     = assetClassValueOf value poolX
    y     = assetClassValueOf value poolX
    lq    = maxLqCap - assetClassValueOf value poolLq

data PoolDiff = PoolDiff
  { diffX         :: Integer
  , diffY         :: Integer
  , diffLiquidity :: Integer
  }

{-# INLINABLE diffPoolState #-}
diffPoolState :: PoolState -> PoolState -> PoolDiff
diffPoolState s0 s1 =
    PoolDiff dx dy dlq
  where
    rx0 = reservesX s0
    rx1 = reservesX s1
    ry0 = reservesY s0
    ry1 = reservesY s1
    lq0 = liquidity s0
    lq1 = liquidity s1
    dx  = rx1 - rx0
    dy  = ry1 - ry0
    dlq = lq0 - lq1 -- pool keeps only the negative part of LQ tokens

{-# INLINABLE getPoolInput #-}
getPoolInput :: ScriptContext -> AssetClass -> TxOut
getPoolInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}} poolNft =
    case findPoolInput' txInfoInputs of
      Just pin -> txInInfoResolved pin
      _        -> traceError "pool input not found"
  where
    findPoolInput' = find ((\v -> assetClassValueOf v poolNft == 1) . txOutValue . txInInfoResolved)

{-# INLINABLE findPoolConfig #-}
findPoolConfig :: TxInfo -> DatumHash -> PoolConfig
findPoolConfig info h = case findDatum h info of
  Just (Datum d) -> case fromBuiltinData d of
    (Just ps) -> ps
    _         -> traceError "error decoding pool data"
  _         -> traceError "pool input datum not found"

{-# INLINABLE validDeposit #-}
validDeposit :: PoolState -> PoolDiff -> Bool
validDeposit PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal amount of liquidity forged" (diffLiquidity <= liquidityUnlocked)
  where liquidityUnlocked = min (divide (diffX * liquidity) reservesX) (divide (diffY * liquidity) reservesY)

{-# INLINABLE validRedeem #-}
validRedeem :: PoolState -> PoolDiff -> Bool
validRedeem PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal redeem" fairRedeem
  where fairRedeem = diffX * liquidity >= diffLiquidity * reservesX && diffY * liquidity >= diffLiquidity * reservesY

{-# INLINABLE validSwap #-}
validSwap :: PoolConfig -> PoolState -> PoolDiff -> Bool
validSwap PoolConfig{..} PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal swap" fairSwap &&
    traceIfFalse "Liquidity emission must not change" (diffLiquidity == zero)
  where
    feeDen   = 1000
    fairSwap =
      if diffX > zero then
        reservesY * diffX * poolFeeNum >= negate diffY * (reservesX * feeDen + diffX * poolFeeNum)
      else
        reservesX * diffY * poolFeeNum >= negate diffX * (reservesY * feeDen + diffY * poolFeeNum)

{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolConfig -> PoolAction -> ScriptContext -> Bool
mkPoolValidator ps0@PoolConfig{..} action ctx =
    traceIfFalse "Pool NFT not preserved" poolNftPreserved &&
    traceIfFalse "Pool settings not preserved" poolSettingsPreserved &&
    traceIfFalse "Assets qty not preserved" strictAssets &&
    traceIfFalse "Script not preserved" scriptPreserved &&
    traceIfFalse "Invalid action" validAction
  where
    self = txInInfoResolved $ case findOwnInput ctx of
      Just poolIn -> poolIn
      _           -> traceError "pool input not found"

    successor = case getContinuingOutputs ctx of
      [pout] -> pout
      _      -> traceError "invalid pool output"

    poolNftPreserved = assetClassValueOf (txOutValue successor) poolNft == 1

    selfDh = case txOutDatum self of
      Just h -> h
      _      -> traceError "pool input datum hash not found"

    successorDh = case txOutDatum successor of
      Just h -> h
      _      -> traceError "pool output datum hash not found"

    poolSettingsPreserved = successorDh == selfDh

    s0   = readPoolState ps0 self
    s1   = readPoolState ps0 successor
    diff = diffPoolState s0 s1

    strictAssets = numAssets <= 5
      where numAssets = length $ flattenValue (txOutValue successor)

    scriptPreserved = txOutAddress successor == txOutAddress self

    validAction = case action of
      Deposit -> validDeposit s0 diff
      Redeem  -> validRedeem s0 diff
      Swap    -> validSwap ps0 s0 diff
