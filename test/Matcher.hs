module Matcher where

import Plutus.V1.Ledger.Api (ExBudget)
import Data.Text (Text)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program(..))
import qualified UntypedPlutusCore.Core.Type as RR
import qualified PlutusCore as PLC
import Plutus.V1.Ledger.Scripts

matchProgramm :: Either ScriptError (Program DeBruijn DefaultUni DefaultFun ()) -> Bool
matchProgramm (Right (Program _ _ (RR.Constant _ (PLC.Some r4)))) = checkList $ words $ show r4
matchProgramm _ = False

checkList :: [String] -> Bool
checkList [_, _, res] = res == "True"
checkList _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight  _        = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft  _       = False