module Minichrome.UI.Components.FrameContainer
  ( frameContainer
  ) where

import Prelude

import Data.Maybe as Maybe
import Effect.Aff.Class as AffClass
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Events as HalogenEvents
import Record as Record

import Minichrome.Config as Config
import Minichrome.UI.Components.Frame as Frame
import Minichrome.UI.State as State

frameContainer
  :: forall m. AffClass.MonadAff m
  => Config.Config
  -> State.State m
  -> Halogen.Component HalogenHTML.HTML State.Query Unit Void m
frameContainer config initialState =
  Halogen.component
    { initialState: const initialState
    , render: render config
    , eval: State.eval config
    , receiver: const Maybe.Nothing
    }

render
  :: forall m
   . Config.Config
  -> State.State m
  -> Halogen.ComponentHTML State.Query
render config state =
  Frame.frame config $ Record.disjointUnion state
    { onPageTitleUpdated: HalogenEvents.input State.UpdateTitle
    , onDidNavigate: HalogenEvents.input State.UpdateAddress
    , onDidNavigateInPage: HalogenEvents.input State.UpdateAddress
    , onNewWindow: HalogenEvents.input State.NewWindow
    , onIPCMessage: HalogenEvents.input State.IPCMessage
    , onExClear: HalogenEvents.input_ State.LeaveEx unit
    , onExEnter: HalogenEvents.input State.RunEx
    , onSearchClear: HalogenEvents.input_ State.CancelSearch unit
    , onSearchEnter: const $ HalogenEvents.input_ State.CommitSearch unit
    , onSearchChange: HalogenEvents.input State.SetSearch
    }
