
import Control.Exception
import Data.Char
import Data.List
import Test.HUnit

import FiniteAutomata

parsingTests = TestList [
    TestCase (assertEqual "Normal parsing" (Transition "a" "b" "c") (parseTransition "a,b,c")),
    TestCase (assertEqual "Epsilon transtion" (Transition "a" "" "c") (parseTransition "a,,c")),
    TestCase (assertThrows "Should fail, incorrect input" (show $ parseTransition "a,,"))]

tests = TestList [TestLabel "parsingTests" parsingTests]

main :: IO ()
main = do
    counts <- runTestTT tests
    putStrLn $ show counts


assertThrows message action = catch (perform action) ((\exception -> if (isPrefixOf "HUnitFailure" (show exception)) then error message else return ())::SomeException -> IO ())

perform action = do
    res <- return action
    assertFailure $ "This input is incorrect:"  ++ res
