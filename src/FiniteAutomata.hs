module FiniteAutomata
( State
, Symbol
, Transition(Transition)
, FA(FA)
, showFA
, parseFA
, processAutomatonFromFile
, processAutomatonFromStdin
, parseTransition
) where

import Data.Char
import Data.List
import System.IO
import System.Environment

type State = String
type Symbol = String

data Transition = Transition {
    leftState :: State,
    symbol :: Symbol,
    rightState :: State
} deriving (Show,Eq,Ord)

data FA = FA {
    states :: [State],
    symbols :: [Symbol],
    transitions :: [Transition],
    startState :: State,
    finishStates :: [State]
} deriving (Show,Eq,Ord)



splitStringBy :: String -> Char -> [String]
splitStringBy input delim = splitStringBy' input delim [] []
    where   splitStringBy' [] _ buffer output = if null buffer then output else output ++ [buffer]
            splitStringBy' (x:xs) delim buffer output = if x == delim then splitStringBy' xs delim [] (output ++ [buffer])
                                                        else splitStringBy' xs delim (buffer ++ [x])  output

concatStrings :: [String] -> String
concatStrings strings = intercalate "," strings

concatStringsWith :: [String] -> String -> String
concatStringsWith strings delim = intercalate delim strings

parseTransition :: String -> Transition
parseTransition line = parseTransition' $ splitStringBy line ','
    where parseTransition' parts = Transition (parts !! 0) (parts !! 1) (parts !! 2)

parseTransitions :: [String] -> [Transition]
parseTransitions lines = map parseTransition lines

parseSymbols :: [String] -> [Symbol]
parseSymbols lines  = map (!!1) (parsedLines lines)
    where parsedLines = map (\x -> (splitStringBy x ','))

parseFA :: [String] -> FA
parseFA lines = FA states symbols transitions startState finishStates
    where
        states =  splitStringBy (lines !! 0) ','
        symbols = parseSymbols $ drop 3 lines
        transitions = parseTransitions $ drop 3 lines
        startState = lines !! 1
        finishStates = splitStringBy (lines !! 2) ','


showFA :: FA -> String
showFA fa = showStates (states fa) ++ "\n" ++ startState fa ++ "\n" ++ showStates (finishStates fa) ++ "\n" ++ showTransitions (transitions fa)
        where
            showStates states = concatStrings states
            showTransitions transitions = concatStringsWith (map showTransition transitions) "\n"
                where showTransition transition = (leftState transition) ++ "," ++ (symbol transition) ++ "," ++ (rightState transition)


printFa :: String -> IO ()
printFa = \content -> putStrLn $ showFA $ parseFA $ lines content

processAutomatonFromFile  :: String -> (String -> IO()) -> IO ()
processAutomatonFromFile fileName operation = do
    handle <- openFile fileName ReadMode
    content <- hGetContents handle
    operation content
    hClose handle

processAutomatonFromStdin :: (String -> IO()) -> IO ()
processAutomatonFromStdin operation = do
    content <- getContents
    operation content