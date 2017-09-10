module Data.GameState where

import Prelude
import Data.Coords
import Data.Tuple (Tuple(..))
import Data.GameItem
import Data.Map as M
import Data.Set (fromFoldable)
import Data.Set as S
import Data.String (joinWith)
import Node.Stream (onFinish)

newtype GameState = GameState
  { items       :: M.Map Coords (S.Set GameItem)
  , player      :: Coords
  , inventory   :: S.Set GameItem
  }


instance showGameState :: Show GameState where
  show (GameState a) = 
    "GameState { "
          <> "\n items: " <> show a.items 
          <> "\n player: " <> show a.player 
          <> "\n inventory: " <> show a.inventory 
          <> "}"
  

emptyState :: GameState
emptyState = GameState { items: M.empty, player: coords 0 0, inventory: S.empty }


initialGameState :: GameState
initialGameState = GameState
  { items      : M.fromFoldable [ Tuple (coords 0 1) (S.singleton Candle)
                                , Tuple (coords 0 0) (S.singleton Matches)
                                ]
  , player     : Coords { x: 0, y: 0 }
  , inventory  : S.empty
  }


addInventory :: GameState -> GameItem -> GameState
addInventory (GameState s) gi = 
  let newInventory = S.insert gi s.inventory in
  let newState = s { inventory = newInventory} in
  GameState newState