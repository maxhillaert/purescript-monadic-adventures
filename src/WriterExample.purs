module WriterExample where
  
import Control.Monad.Writer
import Control.Monad.Writer.Class (tell)
import Data.List 
import Data.Monoid.Additive (Additive(..))
import Prelude (Unit, discard, pure, unit, ($))



type AccIntWriter = Writer (Additive Int) Unit

sumArray :: List Int -> AccIntWriter
sumArray (Cons x xs) = do 
                        tell (Additive x) 
                        sumArray xs
sumArray Nil = pure unit


doSumArray :: List Int -> Int
doSumArray a = 
    let (Additive x) = execWriter $ sumArray a in
    x