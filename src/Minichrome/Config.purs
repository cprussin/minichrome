module Minichrome.Config
  ( Config
  , Keybinding(..)
  , defaultConfig
  , getCommand
  ) where

import Data.Maybe as Maybe

import Minichrome.UI.InputMode as InputMode

-- | This is the type of keybinding definitions--a tuple that maps a `Shortcut`
-- | to an `Action`.
data Keybinding = Keybinding (Array InputMode.Mode) String String

getCommand :: Keybinding -> String
getCommand (Keybinding _ _ command) = command

-- | This `Record` describes the Minichrome config.  Fields are:
-- |
-- | - `port`: The port number to run the HTTP server interface on.
-- | - `browser`: A `Maybe String` that, if `Just`, specifies the external app
-- |              to use when opening links in a new window (e.g. via
-- |              middle-click).  If `Maybe.Nothing`, then links are opened in
-- |              new Minichrome windows.
-- | - `keybindings`: An array of `Keybinding` definitions.
type Config = Record
  ( developerMode :: Boolean
  , port :: Int
  , browser :: Maybe.Maybe String
  , keybindings :: Array Keybinding
  )

-- | The default configuration.
defaultConfig :: Config
defaultConfig =
  { developerMode: false
  , port: 42042
  , browser: Maybe.Nothing
  , keybindings:
    [ Keybinding [ InputMode.Normal, InputMode.Insert ] "C-o" "back"
    , Keybinding [ InputMode.Normal, InputMode.Insert ] "C-i" "forward"
    , Keybinding [ InputMode.Normal ] ":" "ex"
    , Keybinding [ InputMode.Normal ] "h" "left"
    , Keybinding [ InputMode.Normal ] "l" "right"
    , Keybinding [ InputMode.Normal ] "j" "down"
    , Keybinding [ InputMode.Normal ] "k" "up"
    , Keybinding [ InputMode.Normal ] "C-u" "bigUp"
    , Keybinding [ InputMode.Normal ] "C-d" "bigDown"
    , Keybinding [ InputMode.Normal ] "G" "toBottom"
    , Keybinding [ InputMode.Normal ] "g g" "toTop"
    , Keybinding [ InputMode.Normal ] "C-+" "zoomIn"
    , Keybinding [ InputMode.Normal ] "C--" "zoomOut"
    , Keybinding [ InputMode.Normal ] "C-0" "zoomDefault"
    , Keybinding [ InputMode.Insert ] "Escape" "normal"
    ]
  }
