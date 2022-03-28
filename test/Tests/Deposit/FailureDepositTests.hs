{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings      #-}

module Tests.Deposit.FailureDepositTests where

import Plutarch.Api.V1
import PExtra.API
import Plutarch.Prelude
import PExtra.Monadic
import Plutarch

import qualified ErgoDex.Contracts.Pool as Pool
import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PContracts.PDeposit as PDeposit

import Models.PGenerator
import Models.Generator
import Models.Utils
import ErgoDex.PValidators

import Eval
import Matcher
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Api (ExBudget, toData)
import Data.Text (Text)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import qualified Ledger.Ada      as Ada

runFailureIcorrectDepositRedeemer:: Integer -> IO Bool
runFailureIcorrectDepositRedeemer index = do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRefPool poolInOut
    orderInIn  = pgenPoolIn genTxOutRefOrder orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRefOrder
    cxt     = genContext txInfo purpose

    firstIndex  = if (index == 0) then 1 else 0
    secondIndex = if (index == 1) then 0 else 1
    thirdIndex  = if (index == 2) then 0 else 1

    orderRedeem  = genOrderRedeemer firstIndex secondIndex thirdIndex

    cxtToData         = toData cxt
    orderRedeemToData = toData orderRedeem
    orderConfigToData = toData orderConfig

    resOrder = evalWithArgsT (wrapValidator PDeposit.depositValidatorT) [orderConfigToData, orderRedeemToData, cxtToData]
  print resOrder
  return $ isRight $ resOrder

runFailureIcorrectDepositOutValue:: IO Bool
runFailureIcorrectDepositOutValue = do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    maybeLq = genAssetClass cs (genTokenName genIncorrectToken)

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue maybeLq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRefPool poolInOut
    orderInIn  = pgenPoolIn genTxOutRefOrder orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRefOrder
    cxt     = genContext txInfo purpose

    orderRedeem = genOrderRedeemer 0 1 1

    cxtToData         = toData cxt
    orderRedeemToData = toData orderRedeem
    orderConfigToData = toData orderConfig

    resOrder = isRight $ evalWithArgsT (wrapValidator PDeposit.depositValidatorT) [orderConfigToData, orderRedeemToData, cxtToData]
  
  print resOrder
  return resOrder

runFailureIcorrectDepositIdentity :: Integer -> IO Bool
runFailureIcorrectDepositIdentity isCorrect = do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRefPool poolInOut
    orderInIn  = pgenPoolIn genTxOutRefOrder orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = if (isCorrect == 1) then pgenPurpose genTxOutRefOrder else pgenPurpose genTxOutRefPool
    cxt     = genContext txInfo purpose

    orderRedeem = genOrderRedeemer 0 1 1

    cxtToData         = toData cxt
    orderRedeemToData = toData orderRedeem
    orderConfigToData = toData orderConfig

    resOrder = isRight $ evalWithArgsT (wrapValidator PDeposit.depositValidatorT) [orderConfigToData, orderRedeemToData, cxtToData]
  print resOrder
  return resOrder

runFailureIcorrectDepositPoolIdentity :: Integer -> IO Bool
runFailureIcorrectDepositPoolIdentity isCorrect = do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft (if (isCorrect == 1) then 1 else 2), genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRefPool poolInOut
    orderInIn  = pgenPoolIn genTxOutRefOrder orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRefOrder
    cxt     = genContext txInfo purpose

    orderRedeem = genOrderRedeemer 0 1 1

    cxtToData         = toData cxt
    orderRedeemToData = toData orderRedeem
    orderConfigToData = toData orderConfig

    resOrder = isRight $ evalWithArgsT (wrapValidator PDeposit.depositValidatorT) [orderConfigToData, orderRedeemToData, cxtToData]
  print resOrder
  return resOrder

runFailureIcorrectDepositLqReward :: Integer -> IO Bool
runFailureIcorrectDepositLqReward lqAmount = do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq lqAmount, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRefPool poolInOut
    orderInIn  = pgenPoolIn genTxOutRefOrder orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRefOrder
    cxt     = genContext txInfo purpose

    orderRedeem = genOrderRedeemer 0 1 1

    cxtToData         = toData cxt
    orderRedeemToData = toData orderRedeem
    orderConfigToData = toData orderConfig

    resOrder = evalWithArgsT (wrapValidator PDeposit.depositValidatorT) [orderConfigToData, orderRedeemToData, cxtToData]
  print resOrder
  return $ isRight $  resOrder

runFailureIcorrectDepositWithAda :: Integer -> IO Bool
runFailureIcorrectDepositWithAda isXAda = do
  let
    cs    = genCurrencySymbol genCS
    nftTn = genTokenName genNft
    xTn   = genTokenName genX
    yTn   = genTokenName genY
    lqTn  = genTokenName genLQ

    nft = genAssetClass cs nftTn
    x   = if (isXAda == 0) then genAssetClass cs xTn else genAssetClass Ada.adaSymbol Ada.adaToken
    y   = if (isXAda == 0) then genAssetClass cs yTn else genAssetClass Ada.adaSymbol Ada.adaToken
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 100
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 100 pubKeyHashReward 100
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq (genMaxLq - 10), genAdaValue 1000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 1000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq (genMaxLq - 20), genAdaValue 1000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue (1000000 - 300)] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRefPool poolInOut
    orderInIn  = pgenPoolIn genTxOutRefOrder orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRefOrder
    cxt     = genContext txInfo purpose

    orderRedeem = genOrderRedeemer 0 1 1

    cxtToData         = toData cxt
    orderRedeemToData = toData orderRedeem
    orderConfigToData = toData orderConfig

    resOrder = evalWithArgsT (wrapValidator PDeposit.depositValidatorT) [orderConfigToData, orderRedeemToData, cxtToData]
  print resOrder
  return $ isRight $  resOrder