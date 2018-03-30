module Main where

import Data.Char
import Data.List
import System.IO
import System.Environment
import Control.Monad

import FiniteAutomata


zipArgs arguments inputFiles = zipWith (\action inputSpec -> (action,inputSpec)) (mapArguments arguments) (mapInputFiles inputFiles)
    where
        mapArguments = map (\x -> if x == "-t" then (\input -> putStrLn ("t " ++ input)) else (\input -> putStrLn ("i " ++ input)))
        mapInputFiles = map (\input -> if input == "$" then processAutomatonFromStdin else processAutomatonFromFile input)

processArgs args = if (length arguments) > (length inputFiles)
        then  (zipArgs arguments (inputFiles ++ (replicate ((length arguments) - (length inputFiles)) "$")))
        else zipArgs arguments inputFiles
    where arguments = filter (\x -> x == "-t" || x == "-i") args
          inputFiles = filter (\x -> not $ isPrefixOf "-" x) args

main :: IO ()
main = do
     args <- getArgs
     execute $ processArgs ["-t","-i",".gitignore","LICENSE","-t","-i","README.md"]
     where execute [] = return ()
           execute (x:xs) = do
                (snd x) (fst x)
                execute xs



