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


module Dex.Contract.OnChain where

import           Control.Monad          (void)
import           GHC.Generics           (Generic)
import           Ledger.Value           (AssetClass (..), symbols, assetClassValueOf)
import           Ledger.Contexts        (ScriptContext(..))
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import Plutus.Contract
    ( endpoint,
      utxoAt,
      submitTxConstraints,
      submitTxConstraintsSpending,
      collectFromScript,
      select,
      type (.\/),
      BlockchainActions,
      Endpoint,
      Contract,
      AsContractError,
      ContractError )
import           Plutus.Contract.Schema ()
import           Plutus.Trace.Emulator  (EmulatorTrace)
import qualified Plutus.Trace.Emulator  as Trace
import qualified PlutusTx
import           PlutusTx.Prelude
import Ledger
    ( findOwnInput,
      getContinuingOutputs,
      ownHashes,
      ScriptContext(scriptContextTxInfo),
      TxInInfo(txInInfoResolved),
      TxInfo(txInfoInputs),
      DatumHash,
      Redeemer,
      TxOut(txOutDatumHash, txOutValue),
      Value )
import qualified Ledger.Ada             as Ada

import qualified Prelude
import           Schema                 (ToArgument, ToSchema)
import           Wallet.Emulator        (Wallet (..))

import Dex.Types
import Utils

--todo: Refactoring. Check that value of ergo, ada is greather than 0. validate creation, adding ada/ergo to

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE feeNum #-}
feeNum :: Integer
feeNum = 997

{-# INLINABLE lpSupply #-}
-- todo: set correct lp_supply
lpSupply :: Integer
lpSupply = 4000000000

{-# INLINABLE proxyDatumHash #-}
proxyDatumHash :: DatumHash
proxyDatumHash = datumHashFromString "proxyDatumHash"

{-# INLINABLE calculateValueInOutputs #-}
calculateValueInOutputs :: [TxInInfo] -> Coin a -> Integer
calculateValueInOutputs outputs coinValue =
    foldl getAmountAndSum (0 :: Integer) outputs
  where
    getAmountAndSum :: Integer -> TxInInfo -> Integer
    getAmountAndSum acc out = acc + unAmount (amountOf (txOutValue $ txInInfoResolved out) coinValue)

 -- set correct contract datum hash
{-# INLINABLE currentContractHash #-}
currentContractHash :: DatumHash
currentContractHash = datumHashFromString "dexContractDatumHash"

--refactor
{-# INLINABLE inputsLockedByDatumHash #-}
inputsLockedByDatumHash :: DatumHash -> ScriptContext -> [TxInInfo]
inputsLockedByDatumHash hash sCtx = [ proxyInput
                                    | proxyInput <- txInfoInputs (scriptContextTxInfo sCtx)
                                    , txOutDatumHash (txInInfoResolved proxyInput) == Just hash
                                    ]

{-# INLINABLE checkTokenSwap #-}
checkTokenSwap :: ErgoDexPool -> ScriptContext -> Bool
checkTokenSwap ErgoDexPool{..} sCtx =
    traceIfFalse "Expected Ergo or Ada coin to be present in input" inputContainsErgoOrAda &&
    traceIfFalse "Expected correct value of Ergo and Ada in pool output" correctValueSwap
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of ergo dex"

    currentPoolOutput :: TxOut
    currentPoolOutput =
      let
        poolInputs = inputsLockedByDatumHash currentContractHash sCtx
      in
        case poolInputs of
          [input] -> txInInfoResolved input
          otherwise -> traceError "expected exactly one input of ergo dex"

    proxyInputsWithAda :: Integer
    proxyInputsWithAda =
      let
        proxyInputs = inputsLockedByDatumHash proxyDatumHash sCtx
      in calculateValueInOutputs proxyInputs adaCoin

    proxyInputsWithErgo :: Integer
    proxyInputsWithErgo =
      let
        proxyInputs = inputsLockedByDatumHash proxyDatumHash sCtx
      in calculateValueInOutputs proxyInputs ergoCoin

    inputContainsErgoOrAda :: Bool
    inputContainsErgoOrAda =
      let
        input = valueWithin ownInput
        containsErgo = isUnity input adaCoin
        containsAda = isUnity input ergoCoin
      in containsErgo || containsAda

    correctValueSwap :: Bool
    correctValueSwap =
      let
        outputWithValueToSwap = txInInfoResolved ownInput
        isErgoSwap = isUnity (txOutValue outputWithValueToSwap) ergoCoin
        currentAdaValue = outputAmountOf currentPoolOutput adaCoin
        currentErgoValue = outputAmountOf currentPoolOutput ergoCoin
        currentLpValue = outputAmountOf currentPoolOutput lpToken
        newAdaValue = outputAmountOf newOutputWithPoolContract adaCoin
        newErgoValue = outputAmountOf newOutputWithPoolContract ergoCoin
        newLpToken = outputAmountOf newOutputWithPoolContract lpToken
        correctNewAdaValue = if isErgoSwap then currentAdaValue - adaRate proxyInputsWithAda else currentAdaValue + proxyInputsWithAda
        correctNewErgoValue = if isErgoSwap then currentErgoValue + proxyInputsWithErgo else currentErgoValue - ergoRate proxyInputsWithErgo
      in
        newErgoValue == correctNewErgoValue && newAdaValue == correctNewAdaValue && currentLpValue == newLpToken

    -- formula from https://github.com/ergoplatform/eips/blob/eip14/eip-0014.md#simple-swap-proxy-contract

    ergoRate :: Integer -> Integer
    ergoRate adaValueToSwap =
      let
        ergoReserved = outputAmountOf currentPoolOutput ergoCoin
        adaReserved = outputAmountOf currentPoolOutput adaCoin
      in ergoReserved * adaValueToSwap * feeNum `div` (adaReserved * 1000 + adaValueToSwap * feeNum)

    adaRate :: Integer -> Integer
    adaRate ergoValueToSwap =
      let
        ergoReserved = outputAmountOf currentPoolOutput ergoCoin
        adaReserved = outputAmountOf currentPoolOutput adaCoin
      in adaReserved * ergoValueToSwap * feeNum `div` (ergoReserved * 1000 + ergoValueToSwap * feeNum)

    getTrue :: Bool
    getTrue = True

{-# INLINABLE checkCorrectPoolBootstrapping #-}
checkCorrectPoolBootstrapping :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectPoolBootstrapping ErgoDexPool{..} sCtx =
  traceIfFalse "Incorrect conditions of lp token" lpTokenCond &&
  traceIfFalse "Ergo and Ada should be in ouptut" isErgoAndAdaCoinExists
  where

    ownInput :: TxInInfo
    ownInput = findOwnInput' sCtx

    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of ergo dex"

    lpTokenCond :: Bool
    lpTokenCond =
      let
       lpTokenExsit = isUnity (txOutValue newOutputWithPoolContract) lpToken
       lpTokenAmount = outputAmountOf newOutputWithPoolContract lpToken
       adaAmount = outputAmountOf newOutputWithPoolContract adaCoin
       ergoAmount = outputAmountOf newOutputWithPoolContract ergoCoin
       correctLpValue = adaAmount * ergoAmount
      in
        lpTokenExsit && lpTokenAmount * lpTokenAmount >= correctLpValue --check

    isErgoAndAdaCoinExists :: Bool
    isErgoAndAdaCoinExists =
      let
        isErgoExists = isUnity (txOutValue newOutputWithPoolContract) ergoCoin
        isAdaExists = isUnity (txOutValue newOutputWithPoolContract) adaCoin
        adaAmount = outputAmountOf newOutputWithPoolContract adaCoin
        ergoAmount = outputAmountOf newOutputWithPoolContract ergoCoin
      in
        isErgoExists && isAdaExists && adaAmount > 0 && ergoAmount > 0

{-# INLINABLE checkCorrectDepositing #-}
checkCorrectDepositing :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectDepositing ErgoDexPool{..} sCtx =
  traceIfFalse "Incorrect lp token value" checkLpTokenSwap
  where

    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of ergo dex"

    currentPoolOutput :: TxOut
    currentPoolOutput =
      let
        poolInputs = inputsLockedByDatumHash currentContractHash sCtx
      in
        case poolInputs of
          [input] -> txInInfoResolved input
          otherwise -> traceError "expected exactly one input of ergo dex"

    checkLpTokenSwap :: Bool
    checkLpTokenSwap =
      let
        outputToSpent = txInInfoResolved $ findOwnInput' sCtx
        ergoValueToDeposit = outputAmountOf outputToSpent ergoCoin
        adaValueToDeposit = outputAmountOf outputToSpent adaCoin
        currentErgoReserved = outputAmountOf currentPoolOutput ergoCoin
        currentAdaReserved = outputAmountOf currentPoolOutput adaCoin
        currentLpReserved = outputAmountOfcurrentPoolOutput lpToken
        newErgoValue = outputAmountOf newOutputWithPoolContract ergoCoin
        newAdaValue = outputAmountOf newOutputWithPoolContract adaCoin
        prevLpValue = outputAmountOf currentPoolOutput lpToken
        newLpDecValue = outputAmountOf newOutputWithPoolContract lpToken
        correctLpRew = min (ergoValueToDeposit * lpSupply `div` currentErgoReserved) (adaValueToDeposit * lpSupply `div` currentAdaReserved)
      in
        newErgoValue == currentErgoReserved + ergoValueToDeposit &&
        newAdaValue == currentAdaReserved + adaValueToDeposit &&
        newLpDecValue == currentLpReserved - correctLpRew

{-# INLINABLE checkCorrectRedemption #-}
checkCorrectRedemption :: ErgoDexPool -> ScriptContext -> Bool
checkCorrectRedemption ErgoDexPool{..} sCtx =
  traceIfFalse "Incorrect lp token value" True
  where
    newOutputWithPoolContract :: TxOut
    newOutputWithPoolContract = case [ output
                                     | output <- getContinuingOutputs sCtx
                                     , txOutDatumHash output == Just (snd $ ownHashes sCtx)
                                     ] of
      [output]   -> output
      otherwise  -> traceError "expected exactly one output of ergo dex"

    currentPoolOutput :: TxOut
    currentPoolOutput =
      let
        poolInputs = inputsLockedByDatumHash currentContractHash sCtx
      in
        case poolInputs of
          [input] -> txInInfoResolved input
          otherwise -> traceError "expected exactly one input of ergo dex"

    checkLpTokenSwap :: Bool
    checkLpTokenSwap =
      let
        outputToSpent = txInInfoResolved $ findOwnInput' sCtx
        lpRet = outputAmountOf outputToSpent lpToken
        currentErgoReserved = outputAmountOf currentPoolOutput ergoCoin
        currentAdaReserved = outputAmountOf currentPoolOutput adaCoin
        currentLpReserved = outputAmountOf currentPoolOutput lpToken
        newErgoValue = outputAmountOf newOutputWithPoolContract ergoCoin
        newAdaValue = outputAmountOf newOutputWithPoolContract adaCoin
        prevLpValue = outputAmountOf currentPoolOutput lpToken
        newLpDecValue = outputAmountOfnewOutputWithPoolContract lpToken
        correctErgoRew = lpRet * currentErgoReserved `div` lpSupply
        correctAdaRew =  lpRet * currentAdaReserved `div` lpSupply
      in
        newErgoValue == currentErgoReserved - correctErgoRew &&
        newAdaValue == currentAdaReserved - correctAdaRew &&
        newLpDecValue == currentLpReserved + lpRet

{-# INLINABLE mkDexValidator #-}
mkDexValidator :: ErgoDexPool -> ContractAction -> ScriptContext -> Bool
mkDexValidator pool Create sCtx    = checkCorrectPoolBootstrapping pool sCtx
mkDexValidator pool SwapLP sCtx    = checkCorrectRedemption pool sCtx
mkDexValidator pool AddTokens sCtx = checkCorrectDepositing pool sCtx
mkDexValidator pool SwapToken sCtx = checkTokenSwap pool sCtx