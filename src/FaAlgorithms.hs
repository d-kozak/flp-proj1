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
determinizeFA input = det [Data.Set.singleton (startState fa)] (Data.Set.singleton (Data.Set.singleton (startState fa))) (symbols fa) (transitions fa) [] fa
    where fa = removeEpsilonStates input

concatStates :: [State] -> State
concatStates = Data.List.foldl (++) ""

det :: [Set State] -> Set (Set State) -> [Symbol] -> [Transition] -> [Transition] -> FA -> FA
det [] states symbols _ newTransitions fa = FA (toList (Data.Set.map concatStates (Data.Set.map toList states))) symbols newTransitions (startState fa) ( Data.List.map concatStates (Data.List.filter (\list -> any (\item -> elem item (finishStates fa)) list) (Data.List.map toList (toList states))))
det (currentState:nextStates) states symbols oldTransitions transitionBuffer fa = det (nextStates ++ (toList (difference newStates states))) (Data.Set.union states newStates) symbols oldTransitions (transitionBuffer ++ newTransitions) fa
    where   collisionInfo = collisions currentState symbols oldTransitions
            newTransitions = Data.List.map (\(symbol,states) -> Transition (concatStates (toList currentState)) symbol (concatStates (toList states)))  collisionInfo
            newStates = fromList $ Data.List.map (\(symbol,states) -> states) collisionInfo

collisions :: Set State -> [Symbol] -> [Transition] -> [(Symbol, Set State)]
collisions currentState symbols oldTransitions = mapToRightSymbols $ groupBy hasSameSymbol (Data.List.filter (\transition -> member (leftState transition) currentState) oldTransitions)
    where   hasSameSymbol left right = (symbol left) == (symbol right)

mapToRightSymbols :: [[Transition]] -> [(Symbol, Set State)]
mapToRightSymbols groupedTransitions = Data.List.map (\list -> (symbol (list !! 0), (fromList (getRightStates list)))) groupedTransitions
    where getRightStates = Data.List.map rightState

faWithNewTransition :: FA -> [Transition] -> FA
faWithNewTransition fa transitions = FA (states fa) (symbols fa) transitions (startState fa) (finishStates fa)

removeEpsilonStates :: FA -> FA
removeEpsilonStates fa = faWithNewTransition fa (concat $ Data.List.map stateClosureTupleToTransitions epsilonClosures)
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