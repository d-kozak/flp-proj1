
import Control.Exception
import Data.Char
import Data.List
import Test.HUnit
import System.Exit

import FiniteAutomata
import FaAlgorithms

inputParsingTests = TestList [
    TestCase (assertEqual "Normal parsing" (Transition "a" "b" "c") (parseTransition "a,b,c")),
    TestCase (assertEqual "Epsilon transtion" (Transition "a" "" "c") (parseTransition "a,,c")),
    TestCase (assertThrows "Should fail, incorrect input 1" (show $ parseTransition "a,,")),
    TestCase (assertThrows "Should fail, incorrect input 2" (show $ parseTransition "a,b,"))]

simpleEpsilonClosure = TestCase (assertEqual "Simple epsilon closure" ["s","p","q"] (computeEpsilonClosure "s" transitions))
    where transitions = [Transition "s" "" "p", Transition "p" "" "q", Transition "q" "a" "f"]

anotherEpsilonClosures = [  TestCase (assertEqual "Another epsilon closure" ["0","4"] (computeEpsilonClosure "0" transitions)),
                            TestCase (assertEqual "Another epsilon closure" ["1","5"] (computeEpsilonClosure "1" transitions)),
                            TestCase (assertEqual "Another epsilon closure" ["2","6"] (computeEpsilonClosure "2" transitions)),
                            TestCase (assertEqual "Another epsilon closure" ["3"] (computeEpsilonClosure "3" transitions)),
                            TestCase (assertEqual "Another epsilon closure" ["4"] (computeEpsilonClosure "4" transitions)),
                            TestCase (assertEqual "Another epsilon closure" ["5"] (computeEpsilonClosure "5" transitions)),
                            TestCase (assertEqual "Another epsilon closure" ["6"] (computeEpsilonClosure "6" transitions))]
     where transitions = [  Transition "0" "a" "0",
                            Transition "0" "a" "1",
                            Transition "0" "b" "0",
                            Transition "0" "b" "4",
                            Transition "0" "" "4",
                            Transition "1" "a" "4",
                            Transition "1" "a" "5",
                            Transition "1" "b" "2",
                            Transition "1" "" "5",
                            Transition "2" "a" "3",
                            Transition "2" "b" "5",
                            Transition "2" "b" "6",
                            Transition "2" "" "6",
                            Transition "3" "a" "3",
                            Transition "3" "b" "3",
                            Transition "4" "b" "5",
                            Transition "5" "a" "6",
                            Transition "6" "b" "6"]

algorithmsTests = TestList (simpleEpsilonClosure : anotherEpsilonClosures)

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
