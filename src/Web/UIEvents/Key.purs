module Web.UIEvents.Key
  ( module Internal
  , parse
  ) where

import Prelude

import Data.Maybe (Maybe, maybe')
import Web.UIEvents.Key.Details (normalizeKeyString)
import Web.UIEvents.Key.Internal (Key, parseImpl)
import Web.UIEvents.Key.Internal hiding (parseImpl) as Internal

parse :: String -> Maybe Key
parse s = maybe' (\_ -> normalizeKeyString s) pure $ parseImpl s
