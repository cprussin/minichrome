module Minichrome.UI.Components.Webview
  ( Props
  , webview
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Core as HalogenHTMLCore
import Halogen.HTML.Properties as HalogenProperties
import Halogen.HTML.CSS as HalogenCSS
import Node.Electron.Halogen as HalogenElectron

import Minichrome.IPC.PageToUI as IPC
import Minichrome.UI.State as State

type Props p =
  ( address :: String
  , ref :: Halogen.RefLabel
  , onPageTitleUpdated :: String -> Maybe.Maybe (State.Query Unit)
  , onDidNavigate :: String -> Maybe.Maybe (State.Query Unit)
  , onDidNavigateInPage :: String -> Maybe.Maybe (State.Query Unit)
  , onNewWindow :: String -> Maybe.Maybe (State.Query Unit)
  , onIPCMessage :: IPC.Message -> Maybe.Maybe (State.Query Unit)
  , onDidStartLoading :: Maybe.Maybe (State.Query Unit)
  , onDidStopLoading :: Maybe.Maybe (State.Query Unit)
  | p
  )

script :: String
script = "./page.js"

webview :: forall a p. Record (Props p) -> Halogen.HTML a State.Query
webview props =
  HalogenHTML.element (HalogenHTML.ElemName "webview")
    [ style
    , HalogenProperties.ref props.ref
    , HalogenProperties.src props.address
    , HalogenProperties.attr (HalogenHTMLCore.AttrName "preload") script
    , HalogenElectron.onPageTitleUpdated $ _.title >>> props.onPageTitleUpdated
    , HalogenElectron.onDidNavigate $ _.url >>> props.onDidNavigate
    , HalogenElectron.onDidNavigateInPage $ _.url >>> props.onDidNavigateInPage
    , HalogenElectron.onNewWindow $ _.url >>> props.onNewWindow
    , HalogenElectron.onIPCMessage $ IPC.fromEvent >=> props.onIPCMessage
    , HalogenElectron.onDidStartLoading $ const props.onDidStartLoading
    , HalogenElectron.onDidStopLoading $ const props.onDidStopLoading
    ]
    [ ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style $ CSS.flexGrow 1
