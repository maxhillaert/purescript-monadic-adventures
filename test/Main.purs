module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import BalancedParens

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "balanced balanced parens" do
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
    describe "Features" do
      it "runs in NodeJS" $ pure unit
      it "runs in the browser" $ pure unit
      it "supports streaming reporters" $ pure unit
      
      it "is PureScript 0.10.x compatible" $ pure unit
  