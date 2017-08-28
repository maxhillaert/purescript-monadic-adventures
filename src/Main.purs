module Main where

import Prelude
import ReaderExample

import Control.Monad.Eff (Eff)

import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let x = doIt schemeA 0
  log x
