module Node.Electron.Halogen
  ( PageTitleUpdatedEvent
  , DidNavigateEvent
  , NewWindowEvent
  , IPCMessageEvent
  , LoadingEvent
  , onPageTitleUpdated
  , onDidNavigate
  , onDidNavigateInPage
  , onNewWindow
  , onIPCMessage
  , onDidStartLoading
  , onDidStopLoading
  ) where

import Prelude

import Data.Maybe as Maybe
import Halogen.HTML.Events as HalogenEvents
import Halogen.HTML.Properties as HalogenProperties
import Unsafe.Coerce as Unsafe
import Web.Event.Event as Event

type PageTitleUpdatedEvent = Record ( title :: String )
type DidNavigateEvent = Record ( url :: String )
type NewWindowEvent = Record ( url :: String )
type IPCMessageEvent = Record ( channel :: String, args :: Array String )
type LoadingEvent = Record ()

onPageTitleUpdated
  :: forall r i
   . (PageTitleUpdatedEvent -> Maybe.Maybe i)
  -> HalogenProperties.IProp (onPageTitleUpdated :: PageTitleUpdatedEvent | r) i
onPageTitleUpdated = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "page-title-updated")

onDidNavigate
  :: forall r i
   .  (DidNavigateEvent -> Maybe.Maybe i)
  -> HalogenProperties.IProp (onDidNavigate :: DidNavigateEvent | r) i
onDidNavigate = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-navigate")

onDidNavigateInPage
  :: forall r i
   . (DidNavigateEvent -> Maybe.Maybe i)
  -> HalogenProperties.IProp (onDidNavigate :: DidNavigateEvent | r) i
onDidNavigateInPage = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-navigate-in-page")

onNewWindow
  :: forall r i
   . (NewWindowEvent -> Maybe.Maybe i)
  -> HalogenProperties.IProp (onNewWindow :: NewWindowEvent | r) i
onNewWindow = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "new-window")

onIPCMessage
  :: forall r i
   . (IPCMessageEvent -> Maybe.Maybe i)
  -> HalogenProperties.IProp (onIPCMessage :: IPCMessageEvent | r) i
onIPCMessage = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "ipc-message")

onDidStartLoading
  :: forall r i
   . (LoadingEvent -> Maybe.Maybe i)
  -> HalogenProperties.IProp (onDidStartLoading :: LoadingEvent | r) i
onDidStartLoading = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-start-loading")

onDidStopLoading
  :: forall r i
   . (LoadingEvent -> Maybe.Maybe i)
  -> HalogenProperties.IProp (onDidStopLoading :: LoadingEvent | r) i
onDidStopLoading = Unsafe.unsafeCoerce >>>
  HalogenEvents.handler (Event.EventType "did-stop-loading")
