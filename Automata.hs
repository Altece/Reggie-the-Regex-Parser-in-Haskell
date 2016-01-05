module Automata
( State(..)
, Transition(..)
, Termination
, Acceptance
, Transformation
, (&&&)
, nonTerminal
, evaluate) where

-- Type Declarations

data State input output = State (Termination input output) [Transition input output]

data Transition input output = Transition (Acceptance input) (Transformation input) (State input output)

type Termination input output = input -> Maybe output
type Acceptance input = input -> Bool
type Transformation input = input -> input

-- Private Accessors

transitions :: State i o -> [Transition i o]
transitions (State _ ts) = ts

terminatingResult :: i -> State i o -> Maybe o
terminatingResult i (State f _) = f i

condition :: Transition i o -> Acceptance i
condition (Transition t _ _) = t

transformation :: Transition i o -> Transformation i
transformation (Transition _ f _) = f

destination :: Transition i o -> State i o
destination (Transition _ _ s) = s

-- Acceptance Combinator

infixr 3 &&&
(&&&) :: Acceptance a -> Acceptance a -> Acceptance a
f1 &&& f2 = \input -> f1 input && f2 input

-- State Termination

nonTerminal :: Termination i o
nonTerminal _ = Nothing

-- Evaluation

evaluate :: i -> State i o -> [o]
evaluate input state =
  case terminatingResult input state of
    Nothing -> result
    Just o -> o : result
  where result = concatMap eval $ filter pred $ transitions state
        pred t = condition t input
        eval t = evaluate input' $ destination t
          where input' = transformation t input
