{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings      #-}

module Tests.Deposit.SuccessDepositTests where

import Plutarch.Api.V1
import PExtra.API
import Plutarch.Prelude
import PExtra.Monadic
import Plutarch
import Plutus.V1.Ledger.Scripts (ScriptError)

import qualified ErgoDex.Contracts.Pool as Pool
import qualified ErgoDex.PContracts.PPool as PPool
import qualified ErgoDex.PContracts.PDeposit as PDeposit

import Models.PGenerator
import Models.Generator
import Models.Utils

import Eval
import Matcher
import ErgoDex.PValidators

import Plutus.V1.Ledger.Api

pPubKeyHashReward1 :: PubKeyHash
pPubKeyHashReward1 = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f"

currencySymbolName :: CurrencySymbol
currencySymbolName = "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

xTn1 :: TokenName
xTn1 = mkTokenNameHex "6572676f54657374546f6b656e41"

yTn1 :: TokenName
yTn1 = mkTokenNameHex "6572676f54657374546f6b656e42"

nftTn1 :: TokenName
nftTn1 = mkTokenNameHex "6572676f54657374546f6b656e4e4654"

lpTn1 :: TokenName
lpTn1 = mkTokenNameHex "6572676f54657374546f6b656e4c50"

runSuccessDeposit :: IO (Either ScriptError ())
runSuccessDeposit = do
  let
    cs    = currencySymbolName
    nftTn = nftTn1
    xTn   = xTn1
    yTn   = yTn1
    lqTn  = lpTn1

    nft = genAssetClass cs nftTn
    x   = genAssetClass cs xTn
    y   = genAssetClass cs yTn
    lq  = genAssetClass cs lqTn

    poolConfig  = genPoolConfig nft x y lq 1
    ppoolConfig = pconstant poolConfig
    poolDatum   = genDatum poolConfig
    poolDH      = genDatumHash poolDatum

    orderConfig  = genDepositConfig nft x y lq 2 pPubKeyHashReward1 1
    orderDatum   = genOrderDatum orderConfig
    orderDH      = genDatumHash orderDatum

    poolInValue  = genValues [genValue nft 1, genValue x 10, genValue y 10, genValue lq 9223372036854775797, genAdaValue 5000000] mempty
    orderInValue = genValues [genValue x 10, genValue y 10, genAdaValue 5000000] mempty

    poolOutValue  = genValues [genValue nft 1, genValue x 20, genValue y 20, genValue lq 9223372036854775787, genAdaValue 3000000] mempty
    orderOutValue = genValues [genValue lq 10, genAdaValue 2482704] mempty 

    poolInOut  = pgenPoolOut poolDH poolInValue pgenPoolValidator
    orderInOut = pgenPoolOut orderDH orderInValue pgenDepositValidator
    poolInIn   = pgenPoolIn genTxOutRef poolInOut
    orderInIn  = pgenPoolIn genTxOutRef orderInOut

    poolOut  = pgenPoolOut poolDH poolOutValue pgenPoolValidator
    orderOut = pgenOrderOut orderDH orderOutValue pubKeyHashReward

    txInfo  = pgenTxInfo poolInIn orderInIn poolOut orderOut
    purpose = pgenPurpose genTxOutRef
    cxt     = genContext txInfo purpose

    orderRedeem  = genOrderRedeemer 0 1 1
    poolRedeem = genPoolRedeemer 0 Pool.Deposit

    cxtToData         = toData cxt
    orderRedeemToData = toData orderRedeem
    orderConfigToData = toData orderConfig

    poolRedeemToData = toData poolRedeem
    poolConfigToData = toData poolConfig

    resOrder = evalWithArgs (wrapValidator PDeposit.depositValidatorT) [orderConfigToData, orderRedeemToData, cxtToData]
    resPool = evalWithArgs (wrapValidator PPool.poolValidatorT) [poolConfigToData, poolRedeemToData, cxtToData]
  return $ eraseRight resOrder