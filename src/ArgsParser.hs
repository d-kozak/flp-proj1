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

determineFA :: String -> IO ()
determineFA input = putStrLn $ showFA $ parseFA $ lines input

justPrintFA :: String -> IO ()
justPrintFA input = putStrLn $ showFA $ parseFA $ lines input

zipArgs arguments inputFiles = zipWith (\action inputSpec -> inputSpec action) (mapArguments arguments) (mapInputFiles inputFiles)
    where
        mapArguments = map stringToAction
            where   stringToAction "-t" = determineFA
                    stringToAction "-i" = justPrintFA
                    stringToAction arg = error $ "unknown argument: " ++ arg
        mapInputFiles = map stringToAction
            where   stringToAction "/" = processAutomatonFromStdin
                    stringToAction fileName = processAutomatonFromFile fileName

processArgs args = if (length arguments) > (length inputFiles)
        then  (zipArgs arguments (inputFiles ++ (replicate ((length arguments) - (length inputFiles)) "/")))
        else zipArgs arguments inputFiles
    where
          isArgument = isPrefixOf "-"
          arguments = filter (\x ->  isArgument x) args
          inputFiles = filter (\x -> not (isArgument x)) args

