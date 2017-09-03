module TransformersExample where 

import Control.Alternative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.MonadPlus
import Data.Identity
import Data.Maybe
import Data.String
import Data.String
import Data.Traversable
import Data.Tuple
import Data.List.Types
import Prelude

import Control.Monad.Except (ExceptT(..), runExcept, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Either.Nested (in1)
import Data.Identity (Identity(..))
import Data.List (List(..), many, some)
import Data.List.NonEmpty (concat)
import Node.Yargs (yargs)
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



type ParserError = Array String

type ParserLog = Array String

type Parser = StateT String (WriterT ParserLog (ExceptT ParserError Identity))

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

xxx :: StateT String (WriterT (Array String) (ExceptT (Array String) Identity)) String
xxx = do
    s1 <- split
    tell ["s1 = " <> s1]
    s2 <- split
    tell ["s2 = " <> s2]
    pure s2

string :: String -> Parser String
string a = do
  s <- get
  tell ["The state is " <> show s]
  case stripPrefix (Pattern a) s of
    Nothing -> throwError ["Could not match string " <> a <> " at string " <> s]
    Just m -> do
      put m
      pure a

upper :: Parser String 
upper = do 
    s <- split
    guard $ toUpper s == s
    pure s

lower :: Parser String
lower = do
  s <- split
  guard $ toLower s == s
  pure s

upperOrLower :: Parser (List String)
upperOrLower = some upper <|> some lower

runParser :: forall a. Parser a -> String -> Either ParserError (Tuple (Tuple a String) (ParserLog))
runParser m s = 
    let Identity(r) = runExceptT $ runWriterT $ runStateT m s in 
    r

matchManyAsBs :: Parser (List String)
matchManyAsBs = do 
    a <- some (string "a") 
    b <- some (string "b")
    pure (append a b)

asOrBs :: Parser (List String) 
asOrBs = many ((string "a") <|> (string "b"))
    

manyExample = runParser (many split) "boat"

upperExample = runParser (many upper) "BOat"

upperOrLowerExample = runParser upperOrLower "BOatB"

matchManyAsBsExample = runParser matchManyAsBs "aaabbb"






