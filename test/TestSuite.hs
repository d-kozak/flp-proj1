
import Control.Exception
import Data.Char
import Data.List
import Test.HUnit
import System.Exit

import FiniteAutomata

inputParsingTests = TestList [
    TestCase (assertEqual "Normal parsing" (Transition "a" "b" "c") (parseTransition "a,b,c")),
    TestCase (assertEqual "Epsilon transtion" (Transition "a" "" "c") (parseTransition "a,,c")),
    TestCase (assertThrows "Should fail, incorrect input 1" (show $ parseTransition "a,,")),
    TestCase (assertThrows "Should fail, incorrect input 2" (show $ parseTransition "a,b,"))]

algorithmsTests = TestList []

tests = TestList    [TestLabel "parsingInput" inputParsingTests,
                     TestLabel "FaAlgorithms" algorithmsTests]

main :: IO ()
main = do
    counts <- runTestTT tests
    if((errors counts) > 0 || (failures counts) > 0) then
        exitWith $ ExitFailure 1
    else
        exitWith ExitSuccess


assertThrows message action = catch (perform action) ((\exception -> if (isPrefixOf "HUnitFailure" (show exception)) then error message else return ())::SomeException -> IO ())

perform action = do
    res <- return action
    assertFailure $ "This input is incorrect:"  ++ res
