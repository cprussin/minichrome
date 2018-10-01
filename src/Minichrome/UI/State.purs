module Minichrome.UI.State
  ( Query(..)
  , State
  , MessagelineInput(..)
  , eval
  , initialState
  ) where

import Prelude

import Control.Monad.Maybe.Trans as MaybeT
import Control.Monad.State.Class as MonadState
import Data.Array ((..))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.Time.Duration as Duration
import Data.Traversable as Traversable
import Effect as Effect
import Effect.Aff as Aff
import Effect.Aff.Class as AffClass
import Effect.Class as EffectClass
import Effect.Exception as Exception
import Halogen as Halogen
import Halogen.Query.HalogenM as HalogenM
import Node.ChildProcess as ChildProcess
import Node.Electron.Clipboard as Clipboard
import Node.Electron.HTMLWebviewElement as HTMLWebviewElement
import Web.DOM.Element as Element
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.HTMLScriptElement as HTMLScriptElement
import Web.HTML.Window as Window

import Minichrome.Command.Command as Command
import Minichrome.Command.Direction as Direction
import Minichrome.Command.InputMode as InputMode
import Minichrome.Command.YankTarget as YankTarget
import Minichrome.Command.ZoomDirection as ZoomDirection
import Minichrome.Config as Config
import Minichrome.CLI.Client as Client
import Minichrome.IPC.PageToUI as IPCUp
import Minichrome.IPC.UIToPage as IPCDown

type Input = Unit

data MessagelineInput = ExInput | SearchInput

derive instance eqMessagelineInput :: Eq MessagelineInput

type State m = Record
  ( mode :: InputMode.Mode
  , title :: String
  , address :: String
  , webviewAddressAttr :: String
  , position :: Int
  , sequence :: String
  , message :: String
  , searchTerm :: Maybe.Maybe String
  , messageCanceler :: Maybe.Maybe (Exception.Error -> m Unit)
  , webviewRef :: Halogen.RefLabel
  , messagelineInputRef :: Halogen.RefLabel
  , messagelineInput :: Maybe.Maybe MessagelineInput
  , zoomFactor :: Number
  )

data Query a
  = UpdateTitle String a
  | UpdateAddress String a
  | NewWindow String a
  | IPCMessage IPCUp.Message a
  | RunCommand Command.Command a
  | ShowMessage String a
  | RunEx String a
  | LeaveEx a
  | SetSearch String a
  | CancelSearch a
  | CommitSearch a
  | SetSequence String a
  | GetSequence (String -> a)
  | GetCurrentMode (InputMode.Mode -> a)

type DSL m = Halogen.ComponentDSL (State m) Query Void

zoomStep :: Number
zoomStep = 0.1

scrollScale :: Int
scrollScale = 20

-- | This is the state to use when creating the app.
initialState :: forall m. Effect.Effect (State m)
initialState = initialURL <#> \address ->
  { mode: InputMode.Normal
  , title: "minichrome"
  , address: address
  , webviewAddressAttr: address
  , position: 0
  , sequence: ""
  , message: ""
  , searchTerm: Maybe.Nothing
  , messageCanceler: Maybe.Nothing
  , webviewRef: Halogen.RefLabel "webview"
  , messagelineInputRef: Halogen.RefLabel "messagelineInput"
  , messagelineInput: Maybe.Nothing
  , zoomFactor: 1.0
  }

eval :: forall m. AffClass.MonadAff m => Config.Config -> Query ~> DSL m m
eval config = case _ of

  (GetCurrentMode reply) -> reply <$> Halogen.gets _.mode

  (GetSequence reply) -> reply <$> Halogen.gets _.sequence

  (SetSequence str next) -> next <$ Halogen.modify_ _{ sequence = str }

  (ShowMessage message next) -> next <$ showMessage message

  (RunEx cmd next) -> next <$ do
    Halogen.modify_ _{ messagelineInput = Maybe.Nothing }
    Either.either showMessage (runCommand config) (Command.read cmd)

  (LeaveEx next) -> next <$ do
    input <- Halogen.gets _.messagelineInput
    when (input == Maybe.Just ExInput) $ runCommand config Command.LeaveEx

  (CancelSearch next) -> next <$ do
    input <- Halogen.gets _.messagelineInput
    when (input == Maybe.Just SearchInput) do
      runCommand config Command.CloseSearch
      runCommand config Command.CancelSearch

  (CommitSearch next) -> next <$ do
    input <- Halogen.gets _.messagelineInput
    when (input == Maybe.Just SearchInput) $
      runCommand config Command.CloseSearch

  (SetSearch str next) -> next <$ do
    unless (String.null str) $ runCommand config $ Command.Search str

  (UpdateTitle title next) -> next <$ do
    Halogen.modify_ _{ title = title }
    updateWindowTitle

  (UpdateAddress address next) -> next <$ do
    Halogen.modify_ _{ address = address }
    updateWindowTitle

  (NewWindow url next) -> next <$ do
    showMessage $ "Opening " <> url <> " in a new window..."
    Halogen.liftEffect $ case config.browser of
      (Maybe.Just browser) -> void $
        ChildProcess.spawn browser [ url ] ChildProcess.defaultSpawnOptions
      Maybe.Nothing -> Aff.launchAff_ $ Client.browse config url

  (IPCMessage (IPCUp.SetScrollPosition position) next) -> next <$
    Halogen.modify_ _{ position = position }

  (IPCMessage (IPCUp.SetMode mode) next) -> next <$ do
    currentMode <- Halogen.gets _.mode
    when (currentMode /= mode) $ runCommand config $ Command.SetMode mode

  (RunCommand Command.Noop next) -> pure next

  (RunCommand (Command.SetMode mode) next) -> next <$ do
    Halogen.modify_ _{ mode = mode }
    when (mode == InputMode.Normal) $
      withWebviewElement $ flip IPCDown.send IPCDown.Blur >>> Halogen.liftEffect

  (RunCommand (Command.Scroll direction) next) -> next <$
    withWebviewElement \elem ->
      Halogen.liftEffect $ IPCDown.send elem $
        IPCDown.Scroll $ Direction.scale direction scrollScale

  (RunCommand Command.OpenDevTools next) -> next <$
    withWebviewElement \elem ->
      ifM (Halogen.liftEffect $ HTMLWebviewElement.isDevToolsOpened elem)
        (showMessage "Dev tools are already open")
        $ do
          showMessage "Opening dev tools..."
          Halogen.liftEffect $ HTMLWebviewElement.openDevTools elem

  (RunCommand (Command.Zoom direction) next) -> next <$
    (Halogen.gets _.zoomFactor >>= newZoom direction >>> setZoom)

  (RunCommand (Command.Navigate count) next) -> next <$
    if count > 0
      then withWebviewElement \elem -> repeat count $
        ifM (Halogen.liftEffect $ not <$> HTMLWebviewElement.canGoForward elem)
          (showMessage "At end of history")
          $ do
            showMessage "Going forward..."
            Halogen.liftEffect $ HTMLWebviewElement.goForward elem

      else if count < 0 then withWebviewElement \elem -> repeat (-count) $
        ifM (Halogen.liftEffect $ not <$> HTMLWebviewElement.canGoBack elem)
          (showMessage "At beginning of history")
          $ do
            showMessage "Going back..."
            Halogen.liftEffect $ HTMLWebviewElement.goBack elem

      else pure unit

  (RunCommand (Command.Yank YankTarget.URL) next) -> next <$ do
    Halogen.gets _.address >>= Clipboard.writeText >>> Halogen.liftEffect
    showMessage "Copied current URL to clipboard"

  (RunCommand Command.Ex next) -> next <$ setMessagelineInput ExInput

  (RunCommand Command.LeaveEx next) -> next <$ clearMessagelineInput

  (RunCommand Command.StartSearch next) -> next <$
    setMessagelineInput SearchInput

  (RunCommand Command.CloseSearch next) -> next <$ clearMessagelineInput

  (RunCommand Command.CancelSearch next) -> next <$ do
    Halogen.modify_ _{ searchTerm = Maybe.Nothing }
    withWebviewElement \elem -> Halogen.liftEffect $
      HTMLWebviewElement.stopFindInPage elem HTMLWebviewElement.ClearSelection

  (RunCommand (Command.Search term) next) -> next <$ do
    Halogen.modify_ _{ searchTerm = Maybe.Just term }
    withWebviewElement \elem -> Halogen.liftEffect $
      HTMLWebviewElement.findInPage elem term true false

  (RunCommand Command.SearchForward next) -> next <$ searchNext true

  (RunCommand Command.SearchBack next) -> next <$ searchNext false

clearMessagelineInput :: forall m. AffClass.MonadAff m => DSL m m Unit
clearMessagelineInput = Halogen.modify_ _{ messagelineInput = Maybe.Nothing }

setMessagelineInput
  :: forall m
   . AffClass.MonadAff m
  => MessagelineInput
  -> DSL m m Unit
setMessagelineInput mode = do
    cancelMessageCanceler
    clearMessage
    void $ Halogen.modify _{ messagelineInput = Maybe.Just mode }
    elem <- Halogen.gets _.messagelineInputRef >>= Halogen.getHTMLElementRef
    Halogen.liftEffect $
      Maybe.maybe
        mempty
        (HTMLInputElement.toHTMLElement >>> HTMLElement.focus)
        (elem >>= HTMLInputElement.fromHTMLElement)

searchNext :: forall m. AffClass.MonadAff m => Boolean -> DSL m m Unit
searchNext forwards =
  Halogen.gets _.searchTerm >>=
    Maybe.maybe (showMessage "No search term") \term ->
      withWebviewElement \elem -> Halogen.liftEffect $
        HTMLWebviewElement.findInPage elem term forwards true

repeat :: forall a t. Applicative a => Int -> a t -> a Unit
repeat count = const >>> flip Traversable.traverse (1 .. count) >>> void

-- | Get the initial URL by reading the 'data-url' attribute on the script tag.
initialURL :: Effect.Effect String
initialURL = MaybeT.runMaybeT url >>= Maybe.fromMaybe "" >>> pure
  where
    document = HTML.window >>= Window.document
    currentScript = MaybeT.MaybeT $ document >>= HTMLDocument.currentScript
    urlAttribute = Element.getAttribute "data-url" >>> MaybeT.MaybeT
    url = currentScript >>= HTMLScriptElement.toElement >>> urlAttribute

runCommand
  :: forall m
   . AffClass.MonadAff m
  => Config.Config
  -> Command.Command
  -> DSL m m Unit
runCommand config = Client.exec config >>> Aff.launchAff_ >>> Halogen.liftEffect

foreign import setWindowTitle
  :: HTMLDocument.HTMLDocument
  -> String
  -> Effect.Effect Unit

updateWindowTitle :: forall m. AffClass.MonadAff m => DSL m m Unit
updateWindowTitle = do
  state <- Halogen.get
  Halogen.liftEffect $ HTML.window >>= Window.document >>=
    flip setWindowTitle (state.title <> " - " <> state.address)

showMessage :: forall m. AffClass.MonadAff m => String -> DSL m m Unit
showMessage message = do
  cancelMessageCanceler
  canceler <- HalogenM.fork do
    Halogen.liftAff $ Aff.delay $ Duration.Milliseconds 2000.0
    clearMessage
  Halogen.modify_ _{ message = message, messageCanceler = Maybe.Just canceler }

clearMessage :: forall m t. MonadState.MonadState (State t) m => m Unit
clearMessage =
  Halogen.modify_ _{ message = "", messageCanceler = Maybe.Nothing }

-- | Cancel any existing message clear timeout
cancelMessageCanceler :: forall m. AffClass.MonadAff m => DSL m m Unit
cancelMessageCanceler = do
  messageCanceller <- Halogen.gets _.messageCanceler
  Maybe.maybe (pure unit) Halogen.lift $ messageCanceller <@> Exception.error ""

withWebviewElement
  :: forall m
   . EffectClass.MonadEffect m
  => (HTMLWebviewElement.HTMLWebviewElement -> DSL m m Unit)
  -> DSL m m Unit
withWebviewElement cb = do
  webviewRef <- Halogen.gets _.webviewRef
  elem <- Halogen.getHTMLElementRef webviewRef
  Maybe.maybe (Halogen.liftEffect mempty) cb $
    elem >>= HTMLWebviewElement.fromHTMLElement

setZoom :: forall m. AffClass.MonadAff m => Number -> DSL m m Unit
setZoom zoom' = do
  Halogen.modify_ _{ zoomFactor = zoom' }
  void $ withWebviewElement $
    flip HTMLWebviewElement.setZoomFactor zoom' >>> Halogen.liftEffect

newZoom :: ZoomDirection.ZoomDirection -> Number -> Number
newZoom (ZoomDirection.In step) = (_ + (zoomStep * Int.toNumber step))
newZoom (ZoomDirection.Out step) = (_ - (zoomStep * Int.toNumber step))
newZoom ZoomDirection.Reset = const $ 1.0
