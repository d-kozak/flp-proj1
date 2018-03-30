module FaAlgorithms
(
    determinizeFA,
    removeEpsilonStates,
    computeEpsilonClosure
) where

import FiniteAutomata
import Data.List
import Data.Set

determinizeFA :: FA -> FA
determinizeFA fa = determinizeFA' (Data.Set.singleton (Data.Set.singleton (startState fa))) (Data.Set.singleton (Data.Set.singleton (startState fa))) Data.Set.empty fa

type MacroState = Set State
type MacroTransition = (MacroState,Symbol,MacroState)

determinizeFA' :: Set MacroState -> Set MacroState -> Set MacroTransition -> FA -> FA
determinizeFA' statesToExplore macroStates macroTransitions fa
    | Data.Set.null statesToExplore =  FA newStates (symbols fa) newTransitions (startState fa) newFinalStates
        where   newStates = toList $ Data.Set.map foldMacroState macroStates
                newTransitions = toList $ Data.Set.map (\(leftMacroState,symbol,rightMacroState) -> Transition (foldMacroState leftMacroState) symbol (foldMacroState rightMacroState)) macroTransitions
                newFinalStates = toList $ Data.Set.map foldMacroState $ Data.Set.filter isFinalState macroStates
                    where isFinalState macroState = Data.Set.foldl (\acc current -> acc || (elem current (finishStates fa))) False macroState

determinizeFA' statesToExplore macroStates macroTransitions fa = determinizeFA' (Data.Set.union (deleteAt 0 statesToExplore) newGeneratedStates) (Data.Set.union macroStates newGeneratedStates) (Data.Set.union macroTransitions newTransitions) fa
    where
          currentState = elemAt 0 statesToExplore
          symbolsAndTheirNextStates = Data.List.map (Data.List.foldl (\(_,acc) (sym,curElem) -> (sym,curElem:acc)) ("",[])) $ Data.List.groupBy (\(a,_) (b,_) -> a == b) $ [ (symbol,nextState) |
                                                                startState1 <- (toList currentState),
                                                                symbol <- (symbols fa),
                                                                nextState <- (states fa),
                                                                elem (Transition startState1 symbol nextState) (transitions fa)]
          newTransitions = fromList $ Data.List.map (\(symbol,nextStates) -> (currentState,symbol,fromList nextStates)) symbolsAndTheirNextStates
          newGeneratedStates = difference (fromList $ (Data.List.map (\(_,states) -> fromList states)) symbolsAndTheirNextStates) macroStates


foldMacroState :: MacroState -> State
foldMacroState = (Data.Set.foldl (++) "")

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