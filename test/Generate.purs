module Test.Generate where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as A
import Data.CodePoint.Unicode as U
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as I
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Data.String.Normalize (nfc)
import Data.Tuple (Tuple(..), snd)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync as FS


showCPs :: Array CodePoint -> String
showCPs = S.joinWith "" <<< map (append "\\x" <<< I.toStringAs I.hexadecimal <<< CP.codePointToInt)
showCP' :: String -> String
showCP' = showCPs <<< CP.toCodePointArray
parseKey :: String -> Maybe String
parseKey = S.stripPrefix (S.Pattern "KEY ") >=> S.split (S.Pattern "\t") >>> A.head
parseCategory :: String -> Maybe String
parseCategory = S.stripSuffix (S.Pattern " Keys</h3>") >=> S.split (S.Pattern ">") >>> (A.index <@> 1) >>> map
  (simplifyCategory >>> S.replace (S.Pattern " ") (S.Replacement ""))

parseEither :: String -> Maybe (Either String String)
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

run :: forall e.
  Eff
    ( fs :: FS
    , exception :: EXCEPTION
    , console :: CONSOLE
    | e
    )
    Unit
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
    parseImplBody = S.joinWith "\n" $ justKeys <#> \key ->
      "parseImpl " <> show key <> " = Just " <> key
    unparseBody = S.joinWith "\n" $ justKeys <#> \key ->
      "unparse " <> key <> " = " <> show key
    categoryModule = S.joinWith "\n"
      [ """module Web.UIEvents.Key.Internal.Category where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
"""
      , "data Category\n  = Character\n  | " <> S.joinWith "\n  | " justCategories
      , "derive instance eqCategory :: Eq Category"
      , "derive instance ordCategory :: Ord Category"
      , "derive instance genericCategory :: Generic Category _"
      , "instance showCategory :: Show Category where show = genericShow"
      ]
    keyModule = S.joinWith "\n" $
      [ """module Web.UIEvents.Key.Internal where

import Prelude

import Data.Maybe (Maybe(..))
import Web.UIEvents.Key.Internal.Category (Category(..)) as Category
import Web.UIEvents.Key.Internal.Category (Category())
"""
      , "data Key\n  = Unicode String\n  | F Int\n  | Soft Int\n  | " <> S.joinWith "\n  | " justKeys
      , "derive instance eqKey :: Eq Key"
      , "derive instance ordKey :: Ord Key"
      , ""
      , "category :: Key -> Category"
      , "category (Unicode _) = Category.Character"
      , "category (F _) = Category.Function"
      , "category (Soft _) = Category.Function"
      ] <|>
        (associations <#> \(Tuple key category) ->
          "category " <> key <> " = Category." <> category
        ) <|>
      [ """
parseImpl :: String -> Maybe Key
"""     <> parseImplBody <> """
parseImpl c = Nothing

unparse :: Key -> String
unparse (Unicode c) = c
unparse (F n) = "F" <> show n
unparse (Soft n) = "Soft" <> show n
"""     <> unparseBody ]
  FS.writeTextFile UTF8 "src/Web/UIEvents/Key/Internal/Category.purs" categoryModule
  FS.writeTextFile UTF8 "src/Web/UIEvents/Key/Internal.purs" keyModule
  pure unit

simplifyCategory :: String -> String
simplifyCategory "IME and Composition" = "Composition"
simplifyCategory "General-Purpose Function" = "Function" -- Fn keys
simplifyCategory c = c
