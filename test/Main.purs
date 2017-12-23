module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Test.Generate (run)

main :: Eff _ Unit
main = run >>= logShow
