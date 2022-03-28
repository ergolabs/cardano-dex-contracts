module Main(main) where

import Test.HUnit
import Models.PGenerator
import Eval
import Tests.Deposit.FailureDepositTests
import Tests.Deposit.SuccessDepositTests
import Tests.Deposit.SuccessfullDepositPoolTests
import Tests.Deposit.FailureDepositPoolTests
import Plutarch.Evaluate
import Plutarch (ClosedTerm, compile)
import Models.Generator
import Models.Utils
import qualified Data.Text.Encoding      as E

import Helper (successCase, failureCase, runTest)

main :: IO ()
main = do
  let gen = initBSGenerator
  r <- runTestTT tests
  print r

tests = 
  TestList 
    [ 
    -- Deposit --
      runTest "Check deposit correctness" (successCase "Order contract should return true if input data is correct." runSuccessDeposit)
    --, TestLabel "Check deposit correctness." (TestCase $ runSuccessDeposit >>= (\res -> assertBool "Order contract should return true if input data is correct." (res)))
--    , TestLabel "Check deposit redeemer." (TestCase $ (runFailureIcorrectDepositRedeemer 0) >>= (\res -> assertBool "If poolInIx in redeemer is incorrect the test should fail." (not res)))
--    , TestLabel "Check deposit redeemer." (TestCase $ (runFailureIcorrectDepositRedeemer 1) >>= (\res -> assertBool "If orderInIx in deposit redeemer is incorrect the test should fail." (not res)))
--    , TestLabel "Check deposit redeemer." (TestCase $ (runFailureIcorrectDepositRedeemer 2) >>= (\res -> assertBool "If rewardOutIx in deposit redeemer is incorrect the test should fail." (not res)))
--    , TestLabel "Check deposit reward lq." (TestCase $ runFailureIcorrectDepositOutValue >>= (\res -> assertBool "If lq token in deposit out value is incorrect the test should fail." (not res)))
--    , TestLabel "Check deposit reward lq." (TestCase $ (runFailureIcorrectDepositLqReward 1) >>= (\res -> assertBool "If lq tokens amount is less than min value, the test should not be successful." (not res)))
--    , TestLabel "Check deposit reward lq." (TestCase $ (runFailureIcorrectDepositLqReward genMaxLq) >>= (assertBool "If lq tokens amount is greater than min value, the test should not be successful."))
--    , TestLabel "Check deposit reward lq." (TestCase $ (runFailureIcorrectDepositLqReward 10) >>= (assertBool "If lq tokens amount is eq to min value, the test should be successful."))
--    , TestLabel "Check deposit self identity." (TestCase $ (runFailureIcorrectDepositIdentity 0) >>= (\res -> assertBool "If deposit self identity is incorrect the test should fail." (not res)))
--    , TestLabel "Check deposit self identity." (TestCase $ (runFailureIcorrectDepositIdentity 1) >>= (assertBool "If deposit self identity is correct the test should be successfull."))
--    , TestLabel "Check deposit pool identity." (TestCase $ (runFailureIcorrectDepositPoolIdentity 0) >>= (\res -> assertBool "If deposit pool identity is incorrect the test should fail." (not res)))
--    , TestLabel "Check deposit pool identity." (TestCase $ (runFailureIcorrectDepositPoolIdentity 1) >>= (assertBool "If deposit pool identity is correct the test should be successfull."))
--    , TestLabel "Check deposit with ada." (TestCase $ (runFailureIcorrectDepositWithAda 0) >>= (assertBool "If deposit has x as ada all should be correct"))
--    , TestLabel "Check deposit with ada." (TestCase $ (runFailureIcorrectDepositWithAda 1) >>= (assertBool "If deposit has y as ada all should be correct"))
--
--    -- Pool deposit --
--    , TestLabel "Check pool deposit correctness." (TestCase $ runSuccessDepositePool >>= (assertBool "Pool(deposit) contract should return true if input data is correct."))
--    , TestLabel "Check pool deposit redeemer." (TestCase $ (runFailurePoolDepositIncorrectRedeem 0) >>= (\res -> assertBool "If pool index is incorrect the contract should fail." (not res)))
--    , TestLabel "Check pool deposit redeemer." (TestCase $ (runFailurePoolDepositIncorrectRedeem 1) >>= (\res -> assertBool "If pool action is incorrect the contract should fail." (not res)))
--    , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 0) >>= (\res -> assertBool "If X token in pool out value is incorrect the test should fail." (not res))) -- fails
--    , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 1) >>= (\res -> assertBool "If Y token in pool out value is incorrect the test should fail." (not res))) -- fails
--    , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 2) >>= (\res -> assertBool "If Nft token in pool out value is incorrect the test should fail." (not res))) -- fails
--    , TestLabel "Check pool out value." (TestCase $ (runFailurePoolDepositIncorrectValue 3) >>= (\res -> assertBool "If Lq token in pool out value is incorrect the test should fail." (not res)))
--
    ]