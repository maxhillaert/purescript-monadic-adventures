module Data.GameItem where

import Prelude
import Data.Maybe (Maybe(..))

data GameItem = Candle | Matches

derive instance gameItemEq :: Eq GameItem
derive instance gameItemOrd :: Ord GameItem 
instance gameItem :: Show GameItem where
  show :: GameItem -> String
  show a = case a of 
            Candle -> "candle"
            Matches -> "matches"


readItem :: String -> Maybe GameItem
readItem "candle" = Just Candle
readItem "matches" = Just Matches
readItem _ = Nothing

