-- | This module provides some CSS values that are missing from the `CSS`
-- | module.
module Minichrome.UI.CSS
  ( monospace
  , UserSelect
  , userSelect
  , none
  , Cursor
  , cursor
  , defaultCursor
  ) where

import Prelude

import CSS as CSS
import Data.NonEmpty as NonEmpty

-- | The `monospace` generic CSS font-family
monospace :: NonEmpty.NonEmpty Array CSS.GenericFontFamily
monospace =
  NonEmpty.singleton $ CSS.GenericFontFamily $ CSS.fromString "monospace"

-- | A `CSS.Value` for possible values to `user-select`.
newtype UserSelect = UserSelect CSS.Value
derive instance eqUserSelect :: Eq UserSelect
derive instance ordUserSelect :: Ord UserSelect
instance valUserSelect :: CSS.Val UserSelect where
  value (UserSelect u) = u

-- | `user-select: none`.
none :: UserSelect
none = UserSelect $ CSS.fromString "none"

-- | The `user-select` attribute itself.
userSelect :: UserSelect -> CSS.CSS
userSelect = CSS.key $ CSS.fromString "user-select"

-- | A `CSS.Value` for possible values to `cursor`.
newtype Cursor = Cursor CSS.Value
derive instance eqCursor :: Eq Cursor
derive instance ordCursor :: Ord Cursor
instance valCursor :: CSS.Val Cursor where
  value (Cursor u) = u

-- | `cursor: default`.
defaultCursor :: Cursor
defaultCursor = Cursor $ CSS.fromString "default"

-- | The `cursor` attribute itself.
cursor :: Cursor -> CSS.CSS
cursor = CSS.key $ CSS.fromString "cursor"
