module Minichrome.UI.Components.Webview
  ( Query
  , Message(..)
  , webview
  ) where

import Prelude

import CSS as CSS
import Data.Maybe as Maybe
import Halogen as Halogen
import Halogen.HTML as HalogenHTML
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.Properties as HalogenProperties
import Halogen.HTML.CSS as HalogenCSS
import Unsafe.Coerce as Unsafe
import Web.Event.Event as Event

import Minichrome.UI.State as State

type Input = Record
  ( address :: String
  )

data Query a =
  UpdateTitle PageTitleUpdatedEvent a |
  UpdateURL DidNavigateEvent a |
  HandleInput Input a

data Message = TitleUpdated String | URLUpdated String

initialState :: Input
initialState =
  { address: State.initialState.address
  }

type PageTitleUpdatedEvent = Record ( title :: String )

onPageTitleUpdated :: forall r i.
                      (PageTitleUpdatedEvent -> Maybe.Maybe i) ->
                      HalogenProperties.IProp
                        (onPageTitleUpdated :: PageTitleUpdatedEvent | r) i
onPageTitleUpdated = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "page-title-updated")

type DidNavigateEvent = Record ( url :: String )

onDidNavigate :: forall r i.
                 (DidNavigateEvent -> Maybe.Maybe i) ->
                 HalogenProperties.IProp
                   (onDidNavigate :: DidNavigateEvent | r) i
onDidNavigate = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-navigate")

onDidNavigateInPage :: forall r i.
                       (DidNavigateEvent -> Maybe.Maybe i) ->
                       HalogenProperties.IProp
                         (onDidNavigate :: DidNavigateEvent | r) i
onDidNavigateInPage = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-navigate-in-page")

render :: Input -> Halogen.ComponentHTML Query
render input =
  HalogenHTML.element (HalogenHTML.ElemName "webview")
    [ HalogenProperties.src input.address
    , onPageTitleUpdated $ HalogenEvents.input UpdateTitle
    , onDidNavigate $ HalogenEvents.input UpdateURL
    , onDidNavigateInPage $ HalogenEvents.input UpdateURL
    , HalogenCSS.style $ CSS.flexGrow 1
    ]
    [ ]

eval :: forall m. Query ~> Halogen.ComponentDSL Input Query Message m
eval =
  case _ of
    UpdateTitle event next -> do
      Halogen.raise $ TitleUpdated event.title
      pure next
    UpdateURL event next -> do
      Halogen.raise $ URLUpdated event.url
      pure next
    HandleInput n next -> do
      oldN <- Halogen.get
      when (oldN /= n) $ Halogen.put n
      pure next

webview :: forall m. Halogen.Component HalogenHTML.HTML Query Input Message m
webview = Halogen.component
  { initialState: const initialState
  , render
  , eval
  , receiver: HalogenEvents.input HandleInput
  }
