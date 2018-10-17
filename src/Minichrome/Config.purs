module Minichrome.Config
  ( Config
  , ColorSet
  , Keybinding(..)
  , ModelineField(..)
  , defaultConfig
  , getCommand
  ) where

import Prelude

import CSS as CSS
import Data.Array as Array
import Data.Maybe as Maybe

import Minichrome.Command.Command (Command(..))
import Minichrome.Command.Direction (Direction(..))
import Minichrome.Command.InputMode (Mode(..), allModes)
import Minichrome.Command.YankTarget (YankTarget(..))
import Minichrome.Command.ZoomDirection (ZoomDirection(..))

-- | This is the type of keybinding definitions--a tuple that maps a `Shortcut`
-- | to an `Action`.
data Keybinding = Keybinding (Array Mode) String Command

getCommand :: Keybinding -> Command
getCommand (Keybinding _ _ command) = command

data ModelineField
  = ModeIndicator
  | PageTitle
  | PageURL
  | Spacer
  | ScrollPosition

type ColorSet = Record ( bg :: CSS.Color, fg :: CSS.Color )

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
  , modeline :: Record
    ( fields :: Array ModelineField
    , colors :: ColorSet
    , url :: ColorSet
    , messageline :: ColorSet
    , modeIndicator :: Record
      ( normal :: ColorSet
      , insert :: ColorSet
      , follow :: ColorSet
      , select :: ColorSet
      , toggle :: ColorSet
      , av :: ColorSet
      )
    )
  , keybindings :: Array Keybinding
  )

-- | The default configuration.
defaultConfig :: Config
defaultConfig =
  { developerMode: false
  , port: 42042
  , browser: Maybe.Nothing
  , modeline:
    { fields: [ ModeIndicator, PageTitle, PageURL, Spacer, ScrollPosition ]
    , colors: { bg: CSS.darkgrey, fg: CSS.black }
    , url: { bg: CSS.darkgrey, fg: CSS.blue }
    , messageline: { bg: CSS.lightgrey, fg: CSS.black }
    , modeIndicator:
      { normal: { bg: CSS.blue, fg: CSS.white }
      , insert: { bg: CSS.magenta, fg: CSS.white }
      , select: { bg: CSS.magenta, fg: CSS.white }
      , toggle: { bg: CSS.magenta, fg: CSS.white }
      , follow: { bg: CSS.green, fg: CSS.white }
      , av: { bg: CSS.red, fg: CSS.white }
      }
    }
  , keybindings:
    [ Keybinding [ Normal ] "h" $ Scroll $ Left 1
    , Keybinding [ Normal ] "l" $ Scroll $ Right 1
    , Keybinding [ Normal ] "j" $ Scroll $ Down 1
    , Keybinding [ Normal ] "k" $ Scroll $ Up 1
    , Keybinding [ Normal ] "C-u" $ Scroll $ Up 10
    , Keybinding [ Normal ] "C-d" $ Scroll $ Down 10
    , Keybinding [ Normal ] "G" $ Scroll Bottom
    , Keybinding [ Normal ] "g g" $ Scroll Top
    , Keybinding [ Normal ] "y y" $ Yank URL
    , Keybinding [ Normal ] ":" $ Ex
    , Keybinding [ Normal ] "/" $ StartSearch
    , Keybinding [ Normal ] "n" $ SearchForward
    , Keybinding [ Normal ] "N" $ SearchBack
    , Keybinding [ Normal ] "Escape" $ CancelSearch
    , Keybinding [ Normal ] "i" $ SetMode Insert
    , Keybinding [ Normal ] "f" $ SetMode Follow
    , Keybinding [ Normal ] "s" $ SetMode Select
    , Keybinding [ Normal ] "t" $ SetMode Toggle
    , Keybinding [ Normal ] "a" $ SetMode AV
    , Keybinding allModes "C-+" $ Zoom $ In 1
    , Keybinding allModes "C--" $ Zoom $ Out 1
    , Keybinding allModes "C-0" $ Zoom Reset
    , Keybinding allModes "C-o" $ Navigate (-1)
    , Keybinding allModes "C-i" $ Navigate 1
    , Keybinding allModes "C-r" $ Refresh
    , Keybinding allModes "C-R" $ HardRefresh
    , Keybinding (Array.delete Normal allModes) "Escape" $ SetMode Normal
    ]
  }
