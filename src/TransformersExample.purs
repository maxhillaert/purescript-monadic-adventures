module TransformersExample where 

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Identity
import Data.String
import Data.Traversable
import Data.Tuple
import Prelude

import Control.Monad.Except (ExceptT(..), runExcept, throwError)
import Data.Either (Either(..))
import Data.Either.Nested (in1)
import Data.Identity (Identity(..))
import Node.Yargs.Applicative (yarg)


type SafeDivideMonad = ExceptT String Identity Number

safeDivide :: Number -> Number -> SafeDivideMonad
safeDivide x 0.0 = throwError "can't divide by zero"
safeDivide x y = pure (x / y)

runSafeDivide :: SafeDivideMonad -> String
runSafeDivide m = case (runExcept m) of
                    Left a -> a
                    Right a -> show a

type Output = Array String
type WriteOutput = WriterT Output Identity
type Doc = ReaderT Level WriteOutput Unit

type Level = Int


line :: String -> Doc
line s = do 
        level <- ask
        tell [(tabulate level s)] 
    where 
        tabulate :: Level -> String -> String
        tabulate l i | l < 0 = i
        tabulate 0 i = i
        tabulate l i = " " <> tabulate (l-1) i

    
indent :: Doc -> Doc 
indent d = local (\n -> n + 1) d


schemeB :: Doc
schemeB = do
  line "line 1"
  line "line 2"
  indent $ 
    do
    line "line 3"
    line "line 4"
    indent $ 
        do
        line "line 5" 

doDoc r l = 
    let Identity (Tuple _ x) = runWriterT $ runReaderT r l in
    joinWith "\r\n" x