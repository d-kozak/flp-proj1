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
determinizeAndPrintFA input = putStrLn $ renameStatesToNumbers (states detFA) 1 $ showFA detFA 
    where detFA = determinizeFA $ removeEpsilonStates $ parseFA $ lines input


-- pipeline executed when determinizing FA
determinizeAndPrintFANoRenaming :: String -> IO ()
determinizeAndPrintFANoRenaming input = putStrLn $ showFA $ determinizeFA $ removeEpsilonStates $ parseFA $ lines input

-- pipeline executed when only parsing and printing FA
justPrintFA :: String -> IO ()
justPrintFA input = putStrLn $ showFA $ parseFA $ lines input

-- pipeline executed when only removing epsilon states
removeEpsilonStatesFA :: String -> IO ()
removeEpsilonStatesFA input = putStrLn $ showFA $ removeEpsilonStates $ parseFA $ lines input

-- renames states to numbers, starting at 1
renameStatesToNumbers :: [State] -> Integer -> String -> String
renameStatesToNumbers [] counter str = str
renameStatesToNumbers (state:states) counter str = renameStatesToNumbers states  (counter + 1) (replace state (show counter) str)

-- converts arguments and input sources into operations that are later executed                                
zipArgs :: [String] -> [String] -> [IO ()]                                
zipArgs arguments inputFiles = zipWith (\action inputSpec -> inputSpec action) (mapArguments arguments) (mapInputFiles inputFiles)
    where
        mapArguments = map stringToAction
            where   stringToAction "-t" = determinizeAndPrintFA
                    stringToAction "-i" = justPrintFA
                    stringToAction "-e" = removeEpsilonStatesFA
                    stringToAction "-d" = determinizeAndPrintFANoRenaming
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




-- a few functions from standard library, I know this is not a good practise, but I had troubles installing this particular package 

-- just replaces all occurances of string a by string b in string c 
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = mjoin new . split old $ l

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

mjoin :: [a] -> [[a]] -> [a]
mjoin delim l = concat (intersperse delim l)

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

{- | Similar to Data.List.break, but performs the test on the entire remaining
list instead of just one element.
-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

-- splits a list into sublist 
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)
                                

