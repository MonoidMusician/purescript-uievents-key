module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Int as I
import Data.Maybe (maybe)
import Data.String as S
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Node.FS (FS)
import Test.Generate (run)
import Web.UIEvents.Key (Key(..), parse, unparse)

showCPs :: Array CodePoint -> String
showCPs = S.joinWith "" <<< map (append "\\x" <<< I.toStringAs I.hexadecimal <<< CP.codePointToInt)
showCP' :: String -> String
showCP' = showCPs <<< CP.toCodePointArray

logKey :: forall e. Key -> Eff ( console :: CONSOLE | e ) Unit
logKey = case _ of
  Unicode c -> log $ show c <> " " <> showCP' c
  other -> log $ unparse other

main :: Eff ( console :: CONSOLE, fs :: FS, exception :: EXCEPTION ) Unit
-- main = run >>= logShow
main = do
  let logMKey = maybe (log "Nothing") logKey
  logMKey $ parse "a"
  logMKey $ parse "$"
  logMKey $ parse " "
  logMKey $ parse "\n"
  logMKey $ parse "\x0"
  logMKey $ parse "F1"
  logMKey $ parse "Enter"
  run
