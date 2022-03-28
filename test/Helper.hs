module Helper
  ( successCase
  , failureCase
  , runTest
  ) where

import Test.HUnit
import Prelude
import Plutus.V1.Ledger.Scripts (ScriptError)

runTest :: String -> Assertion -> Test
runTest msg assertion = TestLabel msg (TestCase assertion)

successCase :: String -> IO (Either ScriptError ()) -> Assertion
successCase testcaseMsg runResult =
  runResult >>= (\res -> assertBool testcaseMsg (isRight res))

failureCase :: String -> IO (Either ScriptError ()) -> Assertion
failureCase testcaseMsg runResult =
  runResult >>= (\res -> assertBool testcaseMsg (isLeft res))

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight  _        = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft  _       = False