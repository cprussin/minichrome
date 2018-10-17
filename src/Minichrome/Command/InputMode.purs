module Minichrome.Command.InputMode
  ( Mode(..)
  , allModes
  , read
  , help
  , selector
  , modeFor
  ) where

import Prelude

import Data.Either as Either
import Data.Maybe as Maybe
import Data.String as String
import Effect as Effect
import Web.DOM.Element as Element
import Web.DOM.ParentNode as ParentNode

import Minichrome.Temp.DOM as DOM

-- | The `Mode` type enumerates all possible input modes.
data Mode = Normal | Insert | Follow | Select | Toggle | AV

derive instance eqMode :: Eq Mode

instance showMode :: Show Mode where
  show Normal = "normal"
  show Insert = "insert"
  show Follow = "follow"
  show Select = "select"
  show Toggle = "toggle"
  show AV = "av"

help :: String
help = String.joinWith "\n"
  [ "Modes:"
  , "  normal - scroll around, enter other modes, search, etc"
  , "  insert - provide keyboard input"
  , "  follow - follow links and buttons"
  , "  select - change radio or select elements"
  , "  toggle - toggle checkboxes"
  , "  av - control audio and video elements"
  ]

read :: String -> Either.Either String Mode
read "normal" = pure Normal
read "insert" = pure Insert
read "follow" = pure Follow
read "select" = pure Select
read "toggle" = pure Toggle
read "av" = pure AV
read badMode = Either.Left $ "Invalid mode: " <> badMode

allModes :: Array Mode
allModes = [ Normal, Insert, Follow, Select, Toggle, AV ]

selector :: Mode -> Maybe.Maybe ParentNode.QuerySelector
selector Normal = Maybe.Nothing
selector Insert = pure insertSelector
selector Follow = pure followSelector
selector Select = pure selectSelector
selector Toggle = pure toggleSelector
selector AV = pure avSelector

modeFor :: Element.Element -> Effect.Effect Mode
modeFor element =
  ifM (DOM.matches insertSelector element) (pure Insert) $
  ifM (DOM.matches followSelector element) (pure Follow) $
  ifM (DOM.matches selectSelector element) (pure Select) $
  ifM (DOM.matches toggleSelector element) (pure Toggle) $
  ifM (DOM.matches avSelector element) (pure AV) $
  pure Normal

makeSelector :: Array String -> ParentNode.QuerySelector
makeSelector = String.joinWith "," >>> ParentNode.QuerySelector

insertSelector :: ParentNode.QuerySelector
insertSelector = makeSelector
  [ "textarea"
  , "*[contenteditable=true]"
  , "input[type=color]"
  , "input[type=date]"
  , "input[type=datetime]"
  , "input[type=datetime-local]"
  , "input[type=email]"
  , "input[type=file]"
  , "input[type=month]"
  , "input[type=number]"
  , "input[type=password]"
  , "input[type=range]"
  , "input[type=search]"
  , "input[type=tel]"
  , "input[type=text]"
  , "input[type=time]"
  , "input[type=url]"
  , "input[type=week]"
  ]

followSelector :: ParentNode.QuerySelector
followSelector = makeSelector
  [ "a"
  , "button"
  , "input[type=button]"
  , "input[type=image]"
  , "input[type=reset]"
  , "input[type=submit]"
  ]

selectSelector :: ParentNode.QuerySelector
selectSelector = makeSelector
  [ "select"
  , "input[type=radio]"
  ]

toggleSelector :: ParentNode.QuerySelector
toggleSelector = makeSelector
  [ "input[type=checkbox]"
  ]

avSelector :: ParentNode.QuerySelector
avSelector = makeSelector
  [ "audio"
  , "video"
  ]
