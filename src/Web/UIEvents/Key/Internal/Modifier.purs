module Web.UIEvents.Key.Internal.Modifier where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Modifier
  = Alt
  | AltGraph
  | CapsLock
  | Control
  | Fn
  | FnLock
  | Meta
  | NumLock
  | ScrollLock
  | Shift
  | Symbol
  | SymbolLock
  | Hyper
  | Super
derive instance eqModifier :: Eq Modifier
derive instance ordModifier :: Ord Modifier
derive instance genericModifier :: Generic Modifier _
instance showModifier :: Show Modifier where show = genericShow