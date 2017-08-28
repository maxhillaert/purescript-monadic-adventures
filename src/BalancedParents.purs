module BalancedParens where

import Control.Monad.State
import Control.Monad.State.Class
import Data.String
import Data.Tuple
import Prelude

import Data.Either.Nested (in1)



import Data.List (List(..))
import Data.Array (toUnfoldable)


type CounterState = State Int Unit

arrayToList :: forall a. Array a -> List a
arrayToList = toUnfoldable


testParens :: String -> Boolean
testParens "" = true
testParens s = 
    let chs =  toUnfoldable (toCharArray s) in
    let state = processChars chs in
    let r = execState state 0 in
    r == 0
    
    where 
        processChar :: Char -> CounterState
        processChar '(' = modify (\n -> n + 1)
        processChar ')' = modify (\n -> n - 1)
        processChar _ = pure unit

        processChars :: List Char -> CounterState
        processChars (Cons x xs)  = do 
                processChar x 
                processChars xs
        processChars Nil = pure unit



    
