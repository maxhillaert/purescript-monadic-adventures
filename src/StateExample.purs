module StateExample where
  
import Control.Monad.Eff.Console
import Control.Monad.State
import Control.Monad.State.Class
import Control.MonadZero
import Data.List
import Data.Maybe
import Data.Tuple
import Prelude

import Control.Monad.Eff.Exception (name)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (traverse_)
import Data.Traversable (scanl)
import Math (abs)


type Stack = List Number

push :: Number -> State Stack Unit
push n = modify pushI
    where
      pushI :: Stack -> Stack
      pushI a = n : a

pushO :: (Maybe Number) -> State Stack Unit
pushO (Just n) = push n
pushO Nothing = pure unit


pop :: State Stack (Maybe Number)
pop = do
        a <- gets (\n-> head n)
        modify popI
        pure a
    where
        popI :: Stack -> Stack
        popI (Cons x xs) = xs
        popI Nil = Nil 

res = do
   push 5.0
   push 10.0
   push 7.0
   

as = runState res (11.0 : Nil)

res2 = do
    res
    a <- pop 
    pushO $ double <$> a
    where double x = x * 2.0
  
    
as3 = runState res2 (11.0 : Nil)


--testTraverse :: Unit 
--testTraverse = traverse_ print [1, 2, 3]

--sumArray :: Array Number -> State Number Unit
--sumArray a = traverse_ 