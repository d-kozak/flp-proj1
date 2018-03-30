module Main where

import Data.Char
import Data.List
import System.IO
import System.Environment
import Control.Monad

import FiniteAutomata
import ArgsParser


main :: IO ()
main = do
     args <- getArgs
     execute $ processArgs args
     where execute [] = return ()
           execute (x:xs) = do
                x
                if(not (null xs)) then do
                                        putStrLn "=============================="
                                        execute xs
                else execute xs



