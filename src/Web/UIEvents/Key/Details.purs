module Web.UIEvents.Key.Details where

import Prelude

import Data.Array as A
import Data.CodePoint.Unicode as U
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodePoints (toCodePointArray)
import Data.String.Normalize (nfc)
import Data.Tuple (Tuple(..))
import Web.UIEvents.Key.Internal (Key(..))

otherPrinting :: Map String Key
otherPrinting = fromFoldable
  [ Tuple "\n" Enter
  , Tuple "\t" Tab
  , Tuple "\x0008" Backspace
  , Tuple "\x001B" Escape
  , Tuple "\x007F" Delete
  ]

-- From https://www.w3.org/TR/uievents-key/#keys-unicode
verifyKeyString :: String -> Boolean
verifyKeyString = toCodePointArray >>> A.uncons >>> case _ of
  Nothing -> true
  Just { head, tail } ->
    let
      isCombining c | U.isCombining c = true
      isCombining c = U.generalCategory c
        # maybe false (eq U.SpacingCombiningMark)
    in not U.isControl head && A.all isCombining tail

toPrinting :: Key -> String
toPrinting Enter = "\n"
toPrinting Tab = "\t"
toPrinting (Unicode c) = c
toPrinting _ = ""

normalizeKeyString :: String -> Maybe Key
normalizeKeyString s =
  let n = nfc s
  in case lookup n otherPrinting of
    Just k -> Just k
    Nothing | verifyKeyString n -> Just (Unicode n)
            | otherwise -> Nothing
