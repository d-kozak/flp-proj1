-- RKA 2 DKA
-- Autor: David Kozak
-- Contact: dkozak94@gmail.com
-- Year: 2017/2018

-- Module contains functions related to argument parsing
module ArgsParser
(
    processArgs
)
where

import Data.Char
import Data.List
import System.IO
import System.Environment
import Control.Monad

import FiniteAutomata
import FaAlgorithms

-- pipeline executed when determinizing FA
determinizeAndPrintFA :: String -> IO ()
determinizeAndPrintFA input = putStrLn $ showFA $ determinizeFA $ parseFA $ lines input

-- pipeline executed when only parsing and printing FA
justPrintFA :: String -> IO ()
justPrintFA input = putStrLn $ showFA $ parseFA $ lines input

-- pipeline executed when only removing epsilon states
removeEpsilonStatesFA :: String -> IO ()
removeEpsilonStatesFA input = putStrLn $ showFA $ removeEpsilonStates $ parseFA $ lines input

-- renames states to numbers, starting at 1
renameStatesToNumbers :: FA -> FA
renameStatesToNumbers fa = f (states fa) fa "1"
    where   f [] fa counter = fa
            f (x:xs) fa counter = f xs (replaceState x (counter) fa) (counter ++ "1")
                where replaceState previousName newName fa = FA newStates (symbols fa) newTransitions newStartState newFinishStates 
                        where   newStates = map replaceSelectedState (states fa)
                                newTransitions = map mapTransition (transitions fa)
                                    where mapTransition (Transition leftState symbol rightState) = Transition (replaceSelectedState leftState)  symbol  (replaceSelectedState rightState)
                                newStartState = replaceSelectedState (startState fa)
                                newFinishStates = map replaceSelectedState (finishStates fa)
                                replaceSelectedState currentState = if (currentState == previousName) then newName else currentState


-- converts arguments and input sources into operations that are later executed                                
zipArgs :: [String] -> [String] -> [IO ()]                                
zipArgs arguments inputFiles = zipWith (\action inputSpec -> inputSpec action) (mapArguments arguments) (mapInputFiles inputFiles)
    where
        mapArguments = map stringToAction
            where   stringToAction "-t" = determinizeAndPrintFA
                    stringToAction "-i" = justPrintFA
                    stringToAction "-e" = removeEpsilonStatesFA
                    stringToAction arg = error $ "unknown argument: " ++ arg
        mapInputFiles = map stringToAction
            where   stringToAction "/" = processAutomatonFromStdin
                    stringToAction fileName = processAutomatonFromFile fileName

-- splits all args into arguments and input sources
-- if there is less input files then arguments, tries to read from standard input
processArgs args = if (length arguments) > (length inputFiles)
        then  (zipArgs arguments (inputFiles ++ (replicate ((length arguments) - (length inputFiles)) "/")))
        else zipArgs arguments inputFiles
    where
          isArgument = isPrefixOf "-"
          arguments = filter (\x ->  isArgument x) args
          inputFiles = filter (\x -> not (isArgument x)) args

