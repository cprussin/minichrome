module Minichrome.Config
  ( Config
  , Keybinding
  , Shortcut(..)
  , defaultConfig
  ) where

import Data.Maybe as Maybe
import Data.Tuple as Tuple

import Minichrome.Actions as Actions

-- | This is the type of keyboard shortcuts.
data Shortcut = Shortcut String Boolean

-- | This is the type of keybinding definitions--a tuple that maps a `Shortcut`
-- | to an `Action`.
type Keybinding = Tuple.Tuple Shortcut Actions.Action

-- | This `Record` describes the Minichrome config.  Fields are:
-- |
-- | - `port`: The port number to run the HTTP server interface on.
-- | - `browser`: A `Maybe String` that, if `Just`, specifies the external app
-- |              to use when opening links in a new window (e.g. via
-- |              middle-click).  If `Maybe.Nothing`, then links are opened in
-- |              new Minichrome windows.
-- | - `keybindings`: An array of `Keybinding` definitions.
type Config = Record
  ( port :: Int
  , browser :: Maybe.Maybe String
  , keybindings :: Array Keybinding
  )

-- | The default configuration.
defaultConfig :: Config
defaultConfig =
  { port: 42042
  , browser: Maybe.Nothing
  , keybindings:
    [ Tuple.Tuple (Shortcut "ArrowLeft" true) Actions.goBack
    , Tuple.Tuple (Shortcut "ArrowRight" true) Actions.goForward
    ]
  }
