module Test.Main where

import BalancedParens
import Game
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.RWS (RWSResult(..))
import Control.Monad.ST (newSTRef)
import Data.Map as M
import Data.Set as S
import Data.Maybe
import Data.Array
import Data.String as ST
import Test.Spec (pending, describe, it, Spec(..))
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    balancedParensSpec
    gameSpec


balancedParensSpec :: forall r. Spec r Unit
balancedParensSpec = describe "balanced balanced parens" do
      it "supports empty string" do
        let result = testParens ""
        result `shouldEqual` true
      it "supports '(()(())())'" do
        let result = testParens "(()(())())"
        result `shouldEqual` true
      it "supports ')'" do
        let result = testParens ")"
        result `shouldEqual` false
      it "supports '(()()'" do
        let result = testParens "(()()"
        result `shouldEqual` false
      pending "feature complete"



gameSpec :: forall r. Spec r Unit
gameSpec = describe "game" do
      describe "has" do
        it "returns false if not in inventory" do
          let env = GameEnvironment {playerName : "john doe", debugMode : true}
          let state =  emptyState
          let m = has Candle
          let RWSResult gs c s = runGameMonad m env state
          c `shouldEqual` false

        it "returns true if in inventory" do
          let env = GameEnvironment {playerName : "john doe", debugMode : true}
          let GameState state = addInventory emptyState Candle
          let m = has Candle
          let RWSResult gs c l = runGameMonad m env (GameState state)
          c `shouldEqual` true

      describe "pickup" do
        let env = GameEnvironment {playerName : "john doe", debugMode : false} 
        let debugEnv = GameEnvironment {playerName : "john doe", debugMode : true} 
        let GameState state =  emptyState 
        let withCandle = state { items = M.singleton (coords 0 0) (S.singleton Candle)  }
        let withCandleSomewhereElse = state { items = M.singleton (coords 0 1) (S.singleton Candle)  }
        let pickupAndCheck = do 
                    _ <- pickup Candle
                    has Candle
        it "will add item to inventory if it is in the player location" do
          let RWSResult gs c l = runGameMonad pickupAndCheck env (GameState withCandle)
          c `shouldEqual` true
          l `shouldEqual` ["Picked up candle."] 
          let GameState newState = gs 
          (S.member Candle newState.inventory) `shouldEqual` true
          case (M.lookup (coords 0 0) newState.items) of
                    Just xs | S.member Candle xs -> false
                    _ -> true
            `shouldEqual` true

        it "will not add item to inventory if it is not in the player location" do
          let RWSResult gs c l = runGameMonad pickupAndCheck env (GameState withCandleSomewhereElse)
          c `shouldEqual` false
          l `shouldEqual` ["Could not find candle."] 
          let GameState newState = gs 
          (S.member Candle newState.inventory) `shouldEqual` false
          case (M.lookup (coords 0 1) newState.items) of
                    Just xs | S.member Candle xs -> true
                    _ -> false
            `shouldEqual` true

        it "debug mode will log state" do
          let RWSResult gs c l = runGameMonad pickupAndCheck debugEnv (GameState withCandleSomewhereElse)
          case (last l) of 
            Just msg -> ST.take 6 msg
            _ -> ""
            `shouldEqual` "Debug:"
          

  