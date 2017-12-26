module Test.Generate where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array as A
import Data.CodePoint.Unicode as U
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Foldable (for_, oneOfMap)
import Data.Function (on)
import Data.Int as I
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.NonEmpty ((:|))
import Data.String as S
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as CP
import Data.String.Normalize (nfc)
import Data.Tuple (Tuple(..), fst, snd)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync as FS

parseKey :: String -> Maybe String
parseKey = S.stripPrefix (S.Pattern "KEY ") >=> S.split (S.Pattern "\t") >>> A.head
parseCategory :: String -> Maybe String
parseCategory = S.stripSuffix (S.Pattern " Keys</h3>") >=> S.split (S.Pattern ">") >>> (A.index <@> 1) >>> map
  (simplifyCategory >>> S.replace (S.Pattern " ") (S.Replacement ""))

parseEither :: String -> Maybe (Either String String)
parseEither s = Left <$> parseCategory s <|> Right <$> parseKey s

type Endo a = a -> a

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
    modifiers = associations # A.mapMaybe case _ of
      Tuple k "Modifier" -> Just k
      _ -> Nothing

    stripped :: String -> Maybe String
    stripped s = oneOfMap (show >>> S.Pattern >>> (S.stripSuffix <@> s)) (A.reverse $ enumFromTo 0 20)

    groupable :: Maybe String -> Maybe String -> Boolean
    groupable Nothing Nothing = false
    groupable a b = a == b
    grouped = associations # A.groupBy case _, _ of
      Tuple "Key11" _, _ -> false
      _, Tuple "Key12" _ -> false
      Tuple a c1, Tuple b c2 ->
        if c1 == c2
          then (groupable `on` stripped) a b
          else false
    processed = grouped >>= case _ of
      Tuple key cat :| r
        | A.length r > 0
        , Just k <- stripped key
        -> [Tuple (Tuple true k) cat]
      ls -> A.fromFoldable ls <#> \(Tuple key cat) ->
        Tuple (Tuple false key) cat
    processedKeys = processed <#> fst
    mkConstructor (Tuple b key) = key <> guard b " Int"
    mkWildcardPattern (Tuple true key) = "(" <> key <> " _)"
    mkWildcardPattern (Tuple false key) = key
    constructors = processedKeys <#> mkConstructor

    parseImplBody = S.joinWith "\n" $ processedKeys <#> \(Tuple b key) ->
      if not b then "parseImpl " <> show key <> " = Just " <> key
      else "parseImpl s | Just i <- tryParse " <> show key <> " s = Just (" <> key <> " i)"
    unparseBody = S.joinWith "\n" $ processedKeys <#> \(Tuple b key) ->
      if not b then "unparse " <> key <> " = " <> show key
      else "unparse (" <> key <> " i) = " <> show key <> " <> show i"
    fromModifierBody = S.joinWith "\n" $ modifiers <#> \key ->
      "fromModifier Modifier." <> key <> " = " <> key
    toModifierBody = S.joinWith "\n" $ modifiers <#> \key ->
      "toModifier " <> key <> " = Just Modifier." <> key
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
    modifierModule = S.joinWith "\n"
      [ """module Web.UIEvents.Key.Internal.Modifier where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
"""
      , "data Modifier\n  = " <> S.joinWith "\n  | " modifiers
      , "derive instance eqModifier :: Eq Modifier"
      , "derive instance ordModifier :: Ord Modifier"
      , "derive instance genericModifier :: Generic Modifier _"
      , "instance showModifier :: Show Modifier where show = genericShow"
      ]
    keyModule = S.joinWith "\n" $
      [ """module Web.UIEvents.Key.Internal where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String as S
import Web.UIEvents.Key.Internal.Category (Category(..)) as Category
import Web.UIEvents.Key.Internal.Category (Category())
import Web.UIEvents.Key.Internal.Modifier (Modifier(..)) as Modifier
import Web.UIEvents.Key.Internal.Modifier (Modifier())
"""
      , "data Key\n  = Unicode String\n  | " <> S.joinWith "\n  | " constructors
      , "derive instance eqKey :: Eq Key"
      , "derive instance ordKey :: Ord Key"
      , ""
      , "tryParse :: String -> String -> Maybe Int"
      , "tryParse prefix = fromString <=< S.stripPrefix (S.Pattern prefix)"
      , ""
      , "category :: Key -> Category"
      , "category (Unicode _) = Category.Character"
      ] <|>
        (processed <#> \(Tuple key category) ->
          "category " <> mkWildcardPattern key <> " = Category." <> category
        ) <|>
      [ """
parseImpl :: String -> Maybe Key
"""     <> parseImplBody <> """
parseImpl _ = Nothing

unparse :: Key -> String
unparse (Unicode c) = c
"""     <> unparseBody <> """

fromModifier :: Modifier -> Key
"""     <> fromModifierBody <> """

toModifier :: Key -> Maybe Modifier
"""     <> toModifierBody <> """
toModifier _ = Nothing""" ]
  FS.writeTextFile UTF8 "src/Web/UIEvents/Key/Internal/Category.purs" categoryModule
  FS.writeTextFile UTF8 "src/Web/UIEvents/Key/Internal/Modifier.purs" modifierModule
  FS.writeTextFile UTF8 "src/Web/UIEvents/Key/Internal.purs" keyModule
  pure unit

simplifyCategory :: String -> String
simplifyCategory "IME and Composition" = "Composition"
simplifyCategory "General-Purpose Function" = "Function" -- Fn keys
simplifyCategory c = c
