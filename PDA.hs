module PDA
( State
, Transition
, Termination
, Acceptance
, Transformation
, Builder
, acceptPeek
, acceptPrefix
, acceptEpsilon
, prefixTransformation
, popTransformation
, pushTransformation
, builderTransformation
, builderTermination
, evaluate) where

import Data.List
import Data.Maybe
import qualified Automata as A

type Stack a = [a]

push :: a -> Stack a -> Stack a
push a s = a:s

pop :: Stack a -> Stack a
pop (a:s) = s
pop [] = []

peek :: Stack a -> Maybe a
peek (a:s) = Just a
peek [] = Nothing

type Tokens = Stack String
type Input result = (String, Stack String, Maybe result)

type State result = A.State (Input result) result
type Transition result = A.Transition (Input result) result

type Termination r = Input r -> Maybe r
type Acceptance r = Input r -> Bool
type Transformation r = Input r -> Input r

type Builder result = Maybe result -> Maybe result

-- Acceptance Combinators

acceptPeek :: String -> Acceptance r
acceptPeek v (_, st, _) = fromMaybe False $ do peeked <- peek st
                                               return $ v == peeked

acceptPrefix :: String -> Acceptance r
acceptPrefix prefix (s, st, _) = isPrefixOf prefix s

acceptEpsilon :: Acceptance r
acceptEpsilon (s, st, _) = s /= ""

--- Transformation Combinators

prefixTransformation :: String -> Transformation r
prefixTransformation prefix (s, ts, r) = (fromMaybe "" $ stripPrefix prefix s, ts, r)

popTransformation :: Transformation r
popTransformation (s, ts, r) = (s, pop ts, r)

pushTransformation :: String -> Transformation r
pushTransformation t (s, ts, r) = (s, push t ts, r)

builderTransformation :: Builder r -> Transformation r
builderTransformation f (s, ts, r) = (s, ts, f r)

-- State Termination

builderTermination :: Termination r
builderTermination (s, ts, r) = r

-- Evaluation

evaluate :: String -> State result -> [result]
evaluate input = A.evaluate (input, [], Nothing)
