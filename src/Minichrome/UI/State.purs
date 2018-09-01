module Minichrome.UI.State
  ( State
  , initialState
  ) where

import Minichrome.UI.InputMode as InputMode

-- | This record describes the state of the UI application.
type State = Record
  ( mode :: InputMode.Mode
  , title :: String
  , address :: String
  , position :: Int
  )

foreign import initialURL :: String

-- | This is the state to use when creating the app.
initialState :: State
initialState =
  { mode: InputMode.Normal
  , title: "minichrome"
  , address: initialURL
  , position: 0
  }
