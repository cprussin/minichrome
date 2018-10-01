module Minichrome.UI.Components.Frame
  ( Props
  , frame
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.CSS as HalogenCSS
import Halogen.HTML.Properties as HalogenProperties

import Minichrome.Command.InputMode as InputMode
import Minichrome.IPC.PageToUI as IPC
import Minichrome.UI.Components.Messageline as Messageline
import Minichrome.UI.Components.Modeline as Modeline
import Minichrome.UI.Components.Webview as Webview
import Minichrome.UI.State as State

type Props p =
  ( address :: String
  , webviewAddressAttr :: String
  , mode :: InputMode.Mode
  , title :: String
  , position :: Int
  , message :: String
  , sequence :: String
  , webviewRef :: Halogen.RefLabel
  , ex :: Boolean
  , exRef :: Halogen.RefLabel
  , onPageTitleUpdated :: String -> Maybe.Maybe (State.Query Unit)
  , onDidNavigate :: String -> Maybe.Maybe (State.Query Unit)
  , onDidNavigateInPage :: String -> Maybe.Maybe (State.Query Unit)
  , onNewWindow :: String -> Maybe.Maybe (State.Query Unit)
  , onIPCMessage :: IPC.Message -> Maybe.Maybe (State.Query Unit)
  , onExClear :: Maybe.Maybe (State.Query Unit)
  , onExEnter :: String -> Maybe.Maybe (State.Query Unit)
  | p
  )

frame :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
frame props = HalogenHTML.div [ style ] $
  [ webview, Modeline.modeline, Messageline.messageline ] <@> props

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style do
  CSS.height $ CSS.pct 100.0
  CSS.width $ CSS.pct 100.0
  CSS.display $ CSS.flex
  CSS.flexFlow CSS.column CSS.nowrap

webview :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
webview props = Webview.webview
  { address: props.webviewAddressAttr
  , ref: props.webviewRef
  , onPageTitleUpdated: props.onPageTitleUpdated
  , onDidNavigate: props.onDidNavigate
  , onDidNavigateInPage: props.onDidNavigateInPage
  , onNewWindow: props.onNewWindow
  , onIPCMessage: props.onIPCMessage
  }
