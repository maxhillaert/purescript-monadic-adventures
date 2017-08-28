module ReaderExample where
import Control.Monad.Reader
import Data.Char
import Data.String
import Prelude

import Data.Enum (defaultSucc)
import Data.Identity (Identity)
import Data.Traversable (sequence)

type Level = Int
type Doc = Reader Level String

line :: String -> Doc 
line s = do 
        level <- ask
        pure $ tabulate level s
    where 
        tabulate :: Level -> String -> String
        tabulate l i | l < 0 = i
        tabulate 0 i = i
        tabulate l i = " " <> tabulate (l-1) i

    
indent :: Doc -> Doc 
indent d = local (\n -> n + 1) d

cat :: Array Doc -> Doc
cat ds = 
    let joined = joinWith "\r\n" in 
    let seq = sequence ds in
    map joined seq
        
schemeA :: Doc 
schemeA = cat [
    line "line 1",
    line "line 2",
    indent $ cat [
        line "line 3",
        line "line 4"
    ]
]
    

doIt :: Doc -> Level -> String
doIt r l = runReader r l