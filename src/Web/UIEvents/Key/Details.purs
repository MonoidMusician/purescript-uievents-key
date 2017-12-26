module Web.UIEvents.Key.Details where

import Prelude

import Data.Array as A
import Data.CodePoint.Unicode as U
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodePoints (toCodePointArray)
import Data.String.Normalize (nfc)
import Data.Tuple (Tuple(..))
import Web.UIEvents.Key.Internal (Key(..))

otherChars :: Map String Key
otherChars = fromFoldable
  [ Tuple "\\n" Enter
  , Tuple "\\t" Tab
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
      isCombining c = fromMaybe false $
        U.generalCategory c <#> eq U.SpacingCombiningMark
    in not U.isControl head && A.all isCombining tail

normalizeKeyString :: String -> Maybe Key
normalizeKeyString s =
  let n = nfc s
  in case lookup n otherChars of
    Just k -> Just k
    Nothing | verifyKeyString n -> Just (Unicode n)
            | otherwise -> Nothing
