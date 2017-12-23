module Test.Generate where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff.Console (log)
import Data.Array as A
import Data.Char.Unicode as U
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as I
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodePoints as CP
import Data.String.Normalize (nfc)
import Data.Tuple (Tuple(..), snd)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS


showCPs = S.joinWith "" <<< map (append "\\x" <<< I.toStringAs I.hexadecimal <<< CP.codePointToInt)
showCP' = showCPs <<< CP.toCodePointArray
parseKey = S.stripPrefix (S.Pattern "KEY ") >=> S.split (S.Pattern "\t") >>> A.head
parseCategory = S.stripSuffix (S.Pattern " Keys</h3>") >=> S.split (S.Pattern ">") >>> (A.index <@> 1) >>> map
  (simplifyCategory >>> S.replace (S.Pattern " ") (S.Replacement ""))

parseEither s = Left <$> parseCategory s <|> Right <$> parseKey s

type Endo a = a -> a

-- dom-indexed: Interactive :: # Type -> # Type, HTMLdiv :: # Type
-- type Key = SpecialKeys (FunctionKeys ( ... ( "Character" :: String ) ) )
-- type FunctionKeys v = Variant ( "F1" :: Unit, "F2" :: Unit, ..., | v )

{-
myEventHandler = case _ of
  key | keyCategory key /= Function -> Nothing
  F 1 -> Just $ Display "You pressed f1"
  F 2 -> Just $ Display "You pressed f2"
  ...
  F 12 -> Just $ ...
  F 13 -> Just $ ...
  Soft _ -> Just $ Display "Misc. soft key"
  _ -> Nothing
-}

run = do
  source <- FS.readTextFile UTF8 "index-source.txt"
  let
    sourceLines = S.split (S.Pattern "\n") source
    trimmed = S.trim <$> sourceLines
    keyThings = A.mapMaybe (S.stripPrefix (S.Pattern "KEY")) trimmed
    justKeys = A.mapMaybe parseKey trimmed
    justCategories = A.mapMaybe parseCategory trimmed
    stuff = A.mapMaybe parseEither trimmed
    folding :: Either String String -> Endo (Tuple (Maybe String) (L.List (Tuple String String)))
    folding (Left cat) (Tuple _ m) = Tuple (Just cat) m
    folding _ s@(Tuple Nothing _) = s
    folding (Right key) (Tuple (Just cat) m) = Tuple (Just cat) $ L.Cons (Tuple key cat) m
    associations = A.reverse $ A.fromFoldable $ snd $ A.foldl (flip folding) (Tuple Nothing L.Nil) stuff
  log """
import Prelude
import Data.Lens.Prism (Prism', prism)
import Data.CodePoint.Unicode
import Data.String.CodePoints (CodePoint, toCodePointArray, fromCodePointArray)
import Data.Generic.Rep (genericShow, class Generic)
"""
  log $ "data Category\n  = Character\n  | " <> S.joinWith "\n  | " justCategories
  log $ "derive instance eqCategory :: Eq Category"
  log $ "derive instance ordCategory :: Ord Category"
  -- NFC
  log $ "data Key\n  = Unicode String\n  | Fn Int\n  | Soft Int\n  | " <> S.joinWith "\n  | " justKeys
  log $ "derive instance eqKey :: Eq Key"
  log $ "derive instance ordKey :: Ord Key"
  log $ "derive instance genericKey :: Generic Key _"
  log $ "category :: Key -> Category"
  log $ "category (Unicode _) = Character"
  for_ associations \(Tuple key category) ->
    log $ "category " <> key <> " = " <> category
  let
    parseImplBody = S.joinWith "\n" $ justKeys <#> \key ->
      "parseImpl " <> show key <> " = " <> key
    unparseBody = S.joinWith "\n" $ justKeys <#> \key ->
      "unparse " <> key <> " = " <> show key
  log $ """
-- | Space is not given a special name in the spec, match it with `Character " "`
space = Unicode " " :: Key

_Unicode :: Prism' Key String
_Unicode = prism Unicode case _ of
  Unicode s -> Just s
  _ -> Nothing

-- | isSpaceKey = preview _CodePoint >>> maybe false Unicode.isSpace
_CodePoint :: Prism' Key CodePoint
_CodePoint = _Unicode <<< prism (fromCodePointArray <<< pure)
  \s -> case toCodePointArray s of
    [c] -> Just c
    _ -> Nothing

parseImpl :: String -> Maybe Key
parseImpl "Down" = ArrowDown
parseImpl "Up" = ArrowUp
""" <> parseImplBody <> """
parseImpl c = Unicode c

parse :: String -> Maybe Key
parse = parseKeyImpl >>> map normalize

unparse :: Key -> String
unparse (Unicode c) = c
""" <> unparseBody <> """

instance showKey :: Show Key where
  show = genericShow

normalize :: Key -> Key
normalize (Unicode "\\n") = Enter
normalize (Unicode "\\t") = Tab
normalize k = k

validate :: Key -> Maybe Key
validate (Unicode c) = -- verify this is just one character followed by combining characters
"""
  pure unit

simplifyCategory :: String -> String
simplifyCategory "IME and Composition" = "Composition"
simplifyCategory "General-Purpose Function" = "Function" -- Fn keys
simplifyCategory c = c
