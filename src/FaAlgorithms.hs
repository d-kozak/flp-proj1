module FaAlgorithms
(
    determinizeFA,
    computeEpsilonClosure
) where

import FiniteAutomata
import Data.List
import Data.Set

determinizeFA :: FA -> FA
determinizeFA fa = fa

computeEpsilonClosure :: State -> [Transition] -> [State]
computeEpsilonClosure state transitions = Data.List.foldl (++) [state] (Data.List.map (\x -> computeEpsilonClosure x transitions) nextHopEpsilonStates)
    where nextHopEpsilonStates = [(rightState transition) | transition <- transitions, (leftState transition) == state, (symbol transition) == ""]

