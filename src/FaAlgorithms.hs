module FaAlgorithms
(
    determinizeFA,
    det,
    removeEpsilonStates,
    computeEpsilonClosure
) where

import FiniteAutomata
import Data.List
import Data.Set

determinizeFA :: FA -> FA
determinizeFA fa = det [Data.Set.singleton (startState fa)] Data.Set.empty (symbols fa) (transitions fa) [] fa

det :: [Set State] -> Set (Set State) -> [Symbol] -> [Transition] -> [Transition] -> FA -> FA
det [] states symbols _ newTransitions fa = fa
det (currentState:nextStates) states symbols oldTransitions transitionBuffer fa = det (nextStates ++ (toList (difference newStates states))) (Data.Set.union states newStates) symbols oldTransitions (transitionBuffer ++ newTransitions) fa
    where   collisionInfo = collisions currentState symbols oldTransitions
            newTransitions = Data.List.map (\(symbol,states) -> Transition (show currentState) symbol (show states))  collisionInfo
            newStates = fromList $ Data.List.map (\(symbol,states) -> states) collisionInfo

collisions :: Set State -> [Symbol] -> [Transition] -> [(Symbol, Set State)]
collisions currentState symbols oldTransitions = mapToRightSymbols $ groupBy hasSameSymbol (Data.List.filter (\transition -> member (leftState transition) currentState) oldTransitions)
    where   hasSameSymbol left right = (symbol left) == (symbol right)

mapToRightSymbols :: [[Transition]] -> [(Symbol, Set State)]
mapToRightSymbols groupedTransitions = Data.List.map (\list -> (symbol (list !! 0), (fromList (getRightStates list)))) groupedTransitions
    where getRightStates = Data.List.map rightState


removeEpsilonStates :: FA -> [Transition]
removeEpsilonStates fa = concat $ Data.List.map stateClosureTupleToTransitions epsilonClosures
    where   epsilonClosures = Data.List.map (\state -> (state, computeEpsilonClosure state (transitions fa))) (states fa)
            stateClosureTupleToTransitions (state,epsilonClosure) = [
                Transition state (symbol transition) (rightState transition) |
                                                            s <- epsilonClosure,
                                                            transition <- (transitions fa),
                                                            (leftState transition) == s,
                                                            (symbol transition) /= ""]

computeEpsilonClosure :: State -> [Transition] -> [State]
computeEpsilonClosure state transitions = Data.List.foldl (++) [state] (Data.List.map (\x -> computeEpsilonClosure x transitions) nextHopEpsilonStates)
    where nextHopEpsilonStates = [(rightState transition) | transition <- transitions, (leftState transition) == state, (symbol transition) == ""]


faEquals :: FA -> FA -> Bool
faEquals left right =   ((fromList (states left)) == (fromList (states right))) &&
                        ((fromList (symbols left)) == (fromList (symbols right))) &&
                        ((transitionEquals (transitions left) (transitions right))) &&
                        ((startState left) == (startState right)) &&
                        ((fromList (finishStates left)) == (fromList (finishStates right)))

transitionEquals :: [Transition] -> [Transition] -> Bool
transitionEquals left right = (fromList left) == (fromList right)