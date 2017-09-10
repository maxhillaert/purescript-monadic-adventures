module Data.Coords where

import Prelude
import Data.Generic (class Generic, gShow)

newtype Coords = Coords 
  {
    x :: Int,
    y :: Int
  }

derive instance coordsEq :: Eq Coords
derive instance coordsOrd :: Ord Coords
derive instance coordsGeneric :: Generic Coords
instance showCoords :: Show Coords where
  show = gShow


coords :: Int -> Int -> Coords
coords x y = Coords {x:x , y:y}
