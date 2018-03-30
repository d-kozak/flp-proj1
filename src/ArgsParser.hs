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
        mapArguments = map (\x -> if x == "-t" then determineFA else justPrintFA)
        mapInputFiles = map (\input -> if input == "$" then processAutomatonFromStdin else processAutomatonFromFile input)

processArgs args = if (length arguments) > (length inputFiles)
        then  (zipArgs arguments (inputFiles ++ (replicate ((length arguments) - (length inputFiles)) "$")))
        else zipArgs arguments inputFiles
    where arguments = filter (\x -> x == "-t" || x == "-i") args
          inputFiles = filter (\x -> not $ isPrefixOf "-" x) args

