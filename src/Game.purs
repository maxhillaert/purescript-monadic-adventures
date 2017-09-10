module Game where


import Control.Monad.RWS
import Data.Foldable
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Array (fromFoldable) as A
import Data.Either.Nested (in1)
import Data.Functor (mapFlipped)
import Data.Generic (class Generic, gShow)
import Data.Int.Bits (xor)
import Data.Map as M
import Data.Set (fromFoldable)
import Data.Set as S
import Data.String (joinWith)
import Node.Stream (onFinish)
import Data.Coords
import Data.GameEnvironment
import Data.GameItem
import Data.GameState

type Log = Array String

type GameMonad = RWS GameEnvironment Log GameState 

has :: GameItem -> GameMonad Boolean
has item = do 
  GameState state <- get
  pure $ item `S.member` state.inventory

pickup :: GameItem -> GameMonad Unit
pickup item = do
  GameState state <- get
  let itemsNear = M.lookup state.player state.items
  let result = case itemsNear of 
                  Just items | S.member item items -> 
                        do 
                        let newState = state { inventory =  (S.insert item state.inventory),
                                                items = (M.update (\v-> Just (S.delete item v) ) state.player state.items)
                                              }
                        put (GameState newState)
                        tell ["Picked up " <> show item <> "."]
                        pure unit
                  _ -> 
                        tell ["Could not find " <> show item <> "."]
  GameEnvironment env <- ask
  case env.debugMode of 
    true -> do 
              result
              state <- get
              tell ["Debug: " <> show state]
    false -> result


describeRoom :: GameMonad Unit
describeRoom = do
  GameState state <- get
  let roomItems = maybe "Can't see any items here." listItems (M.lookup state.player state.items)
  tell ["I can see the following items: " <> roomItems]
  where
    listItems :: S.Set GameItem -> String
    listItems s = joinWith ", " $ A.fromFoldable $ S.map show s

data Direction = East | West | North | South

instance showDirection :: Show Direction where
  show East = "East"
  show West = "West"
  show North = "North"
  show South = "South"

move :: Direction -> GameMonad Unit
move d = do 
  moveImpl d
  tell ["Moved " <> show d]
  where 
    moveImpl :: Direction -> GameMonad Unit
    moveImpl North = modify $ transformCoords 0 1
    moveImpl South = modify $ transformCoords 0 (-1)
    moveImpl East = modify $ transformCoords 1 0
    moveImpl West = modify $ transformCoords (-1) 0
 
    transformCoords :: Int -> Int -> GameState -> GameState 
    transformCoords x y (GameState gs) = 
      let Coords playerLoc = gs.player in
      let newX = playerLoc.x + x in
      let newY = playerLoc.y + y in 
      GameState gs { player = coords newX newY}


game :: Array String -> GameMonad Unit
game ["look"] = describeRoom
game ["take", thing] = 
  maybe (tell ["Can't see this item here."]) pickup (readItem thing) 
game ["east"] = move East
game ["west"] = move West
game ["north"] = move North
game ["south"] = move South
  
game _ = tell ["I don't understand"]



runGameMonad :: forall a. GameMonad a -> GameEnvironment -> GameState -> (RWSResult GameState a Log)
runGameMonad m r s = runRWS m r s
