{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings      #-}

module Tests.Deposit.SuccessfullDepositPoolTests where

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

runSuccessDepositePool :: IO Bool
runSuccessDepositePool = do
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
    poolInIn   = pgenPoolIn genTxOutRef poolInOut
    orderInIn  = pgenPoolIn genTxOutRef orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRef
    cxt     = genContext txInfo purpose

    poolRedeem = genPoolRedeemer 0 Pool.Deposit

    cxtToData        = toData cxt
    poolRedeemToData = toData poolRedeem
    poolConfigToData = toData poolConfig

    resPool = isRight $ evalWithArgsT (wrapValidator PPool.poolValidatorT) [poolConfigToData, poolRedeemToData, cxtToData]

  return resPool