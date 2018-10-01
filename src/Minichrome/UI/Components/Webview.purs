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

import Minichrome.IPC.PageToUI as IPC
import Minichrome.UI.WebviewEvents as WebviewEvents
import Minichrome.UI.State as State

type Props p =
  ( address :: String
  , ref :: Halogen.RefLabel
  , onPageTitleUpdated :: String -> Maybe.Maybe (State.Query Unit)
  , onDidNavigate :: String -> Maybe.Maybe (State.Query Unit)
  , onDidNavigateInPage :: String -> Maybe.Maybe (State.Query Unit)
  , onNewWindow :: String -> Maybe.Maybe (State.Query Unit)
  , onIPCMessage :: IPC.Message -> Maybe.Maybe (State.Query Unit)
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
    , WebviewEvents.onPageTitleUpdated $ _.title >>> props.onPageTitleUpdated
    , WebviewEvents.onDidNavigate $ _.url >>> props.onDidNavigate
    , WebviewEvents.onDidNavigateInPage $ _.url >>> props.onDidNavigateInPage
    , WebviewEvents.onNewWindow $ _.url >>> props.onNewWindow
    , WebviewEvents.onIPCMessage $ IPC.fromEvent >=> props.onIPCMessage
    ]
    [ ]

style :: forall t p. HalogenProperties.IProp (style :: String | p) t
style = HalogenCSS.style $ CSS.flexGrow 1
