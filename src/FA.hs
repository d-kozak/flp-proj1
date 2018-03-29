module FA
( State
, Symbol
, Transition(Transition)
, FA(FA)
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
