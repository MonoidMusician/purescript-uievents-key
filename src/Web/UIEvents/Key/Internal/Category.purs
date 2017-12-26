module Web.UIEvents.Key.Internal.Category where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Category
  = Character
  | Special
  | Modifier
  | Whitespace
  | Navigation
  | Editing
  | UI
  | Device
  | Composition
  | Function
  | Multimedia
  | MultimediaNumpad
  | Audio
  | Speech
  | Application
  | Browser
  | MobilePhone
  | TV
  | MediaController
derive instance eqCategory :: Eq Category
derive instance ordCategory :: Ord Category
derive instance genericCategory :: Generic Category _
instance showCategory :: Show Category where show = genericShow