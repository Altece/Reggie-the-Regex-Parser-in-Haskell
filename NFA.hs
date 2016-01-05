module NFA
( State
, Transition
, Termination
, Acceptance
, Transformation
, Builder
, acceptEpsilon
, acceptPrefix
, prefixTransformation
, evaluate) where

import Data.List
import Data.Maybe
import qualified Automata as A

type Input result = (String, Maybe result)

type State result = A.State (Input result) result
type Transition result = A.Transition (Input result) result

type Termination r = Input r -> Maybe r
type Acceptance r = Input r -> Bool
type Transformation r = Input r -> Input r
type Builder r = Maybe r -> Maybe r

-- Acceptance Combinators

acceptEpsilon (s, _) = s /= ""

acceptPrefix :: String -> Acceptance r
acceptPrefix prefix (s, _) = isPrefixOf prefix s

-- Transformation Combinators

prefixTransformation :: String -> Transformation r
prefixTransformation prefix (s, r) = (fromMaybe "" $ stripPrefix prefix s, r)

builderTransformation :: Builder r -> Transformation r
builderTransformation f (s, r) = (s, f r)

-- Evaluation

evaluate :: String -> State result -> [result]
evaluate input = A.evaluate (input, Nothing)
