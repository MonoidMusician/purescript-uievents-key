module Web.UIEvents.Key.Prisms where

import Prelude

import Data.Lens (Prism', iso, only, prism')
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (CodePoint, fromCodePointArray, toCodePointArray)
import Web.UIEvents.Key.Internal (Key(..), fromModifier, toModifier)
import Web.UIEvents.Key.Internal.Modifier (Modifier)

-- | Space is not given a special name in the spec, match it with `Character " "`
_space :: Prism' Key Unit
_space = _Unicode <<< only " "

-- | Prism for the `Unicode` constructor of `Key`.
_Unicode :: Prism' Key String
_Unicode = prism' Unicode case _ of
  Unicode s -> Just s
  _ -> Nothing

-- | isWhitespaceKey = preview _CodePoint >>> maybe false Unicode.isSpace
_CodePoint :: Prism' Key CodePoint
_CodePoint = _Unicode <<< iso toCodePointArray fromCodePointArray <<< prism' pure
  case _ of
    [c] -> Just c
    _ -> Nothing

_Modifier :: Prism' Key Modifier
_Modifier = prism' fromModifier toModifier
