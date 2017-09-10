module Main where

import Prelude
import ReaderExample

import Control.Monad.Eff 
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console
import Control.Monad.Eff
import Data.Maybe
import Data.Either
import Node.ReadLine as RL
import Game (runGameMonad, initialGameState, game, GameEnvironment, GameState, gameEnvironment)
import Data.Foldable (for_)
import Control.Monad.RWS (RWSResult(..), runRWS)
import Data.Newtype (wrap)
import Data.String (split)
import Node.Yargs.Applicative (Y, runY, flag, yarg)
import Node.Yargs.Setup (usage)

runGame
  :: forall eff
   . GameEnvironment
  -> Eff ( exception :: EXCEPTION
         , readline :: RL.READLINE
         , console :: CONSOLE
         | eff
         ) Unit
runGame env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " 2 interface
  RL.prompt interface

  --
  let
    lineHandler
      :: GameState
      -> String
      -> Eff ( exception :: EXCEPTION
             , console :: CONSOLE
             , readline :: RL.READLINE
             | eff
             ) Unit
    lineHandler currentState input = do
      let arr = split (wrap " ") input
      let x = game arr
      case runGameMonad x env currentState of
        RWSResult state _ written -> do
          for_ written log
          RL.setLineHandler interface $ lineHandler state
      RL.prompt interface
      pure unit

  RL.setLineHandler interface $ lineHandler initialGameState
  pure unit

main :: forall eff. Eff ( exception :: EXCEPTION
            , console :: CONSOLE
            , readline :: RL.READLINE 
            | eff
            ) Unit
main = runY (usage "$0 -p <player name>") $ map runGame env
  where
  env :: Y GameEnvironment
  env = gameEnvironment <$> yarg "p" ["player"]
                                     (Just "Player name")
                                     (Right "The player name is required")
                                     false
                        <*> flag "d" ["debug"]
                                     (Just "Use debug mode")