module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (traverse_)
import Data.Int as I
import Data.Maybe (maybe)
import Data.Monoid (power)
import Data.String as S
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Node.FS (FS)
import Test.Generate (run)
import Web.UIEvents.Key (Key(..), parse, unparse)

hex4 :: Int -> String
hex4 i =
  let s = I.toStringAs I.hexadecimal i
  in power "0" (4 - S.length s) <> s
showCPs :: Array CodePoint -> String
showCPs = S.joinWith "" <<< map (append "\\x" <<< hex4 <<< CP.codePointToInt)
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
  traverse_ (logMKey <<< parse)
    [ "a"
    , "$"
    , " "
    , "\n"
    , "\x0"
    , "F1"
    , "Enter"
    , "FavoriteStore0"
    , "a", "A", "b", "B", "å", "é", "ü", "ñ"
    , "@", "%", "$", "*", "0", "1", "2"
    , "あ", "日", "中", "一", "二", "三"
    , "ا", "ب", "ة", "ت" , "١", "٢", "٣"
    , "а", "б", "в", "г"
    , "±", "ʶ", "϶", "൹", "℉"
    , "\x1e0d\x0307"
    , "\x0064\x0323\x0307"
    , "\x1e0b\x0323"
    , "\x00f4"
    , "\x006f\x0302"
    ]
  -- run
