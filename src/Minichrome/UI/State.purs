module Minichrome.UI.State
  ( Query(..)
  , State
  , MessagelineInput(..)
  , LoadingState(..)
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
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
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
import Web.DOM.Node as Node
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
import Minichrome.Temp.DOM as DOM

type Input = Unit

data MessagelineInput = ExInput | SearchInput

data LoadingState = Loaded | Loading

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
  , loadingState :: LoadingState
  )

data Query a
  = UpdateTitle String a
  | UpdateAddress String a
  | NewWindow String a
  | IPCMessage IPCUp.Message a
  | RunCommand Command.Command a
  | ShowMessage String a
  | SetLoadingState LoadingState a
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
  , loadingState: Loaded
  }

eval :: forall m. AffClass.MonadAff m => Config.Config -> Query ~> DSL m m
eval config = case _ of

  (GetCurrentMode reply) -> reply <$> Halogen.gets _.mode

  (GetSequence reply) -> reply <$> Halogen.gets _.sequence

  (SetSequence str next) -> next <$ Halogen.modify_ _{ sequence = str }

  (ShowMessage message next) -> next <$ showMessage message

  (SetLoadingState state next) -> next <$
    Halogen.modify_ _{ loadingState = state }

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

  (IPCMessage (IPCUp.SetMode mode) next) -> next <$
    Halogen.modify_ _{ mode = mode }

  (IPCMessage (IPCUp.ShowMessage message) next) -> next <$ showMessage message

  (RunCommand Command.Noop next) -> pure next

  (RunCommand Command.Next next) -> next <$ do
    mode <- Halogen.gets _.mode
    unless (mode == InputMode.Normal) $
      withWebviewElement \elem -> Halogen.liftEffect do
        IPCDown.send elem $ IPCDown.FocusNextForMode mode

  (RunCommand Command.Previous next) -> next <$ do
    mode <- Halogen.gets _.mode
    unless (mode == InputMode.Normal) $
      withWebviewElement \elem -> Halogen.liftEffect do
        IPCDown.send elem $ IPCDown.FocusPreviousForMode mode

  (RunCommand (Command.SetMode mode) next) -> next <$ do
    when (mode == InputMode.Normal) $
      withWebviewElement $ flip IPCDown.send IPCDown.Blur >>> Halogen.liftEffect
    unless (mode == InputMode.Normal) $
      withWebviewElement \elem -> Halogen.liftEffect do
        IPCDown.send elem $ IPCDown.FocusNextForMode mode
        unlessM (isFocused $ HTMLWebviewElement.toNode elem) $
          HTMLElement.focus $ HTMLWebviewElement.toHTMLElement elem

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

      else
        withWebviewElement $ HTMLWebviewElement.reload >>> Halogen.liftEffect

  (RunCommand Command.Refresh next) -> next <$
    withWebviewElement (HTMLWebviewElement.reload >>> Halogen.liftEffect)

  (RunCommand Command.HardRefresh next) -> next <$
    withWebviewElement
      (HTMLWebviewElement.reloadIgnoringCache >>> Halogen.liftEffect)

  (RunCommand (Command.Go url) next) -> next <$
    let newUrl = prefixProtocol url
    in Halogen.modify_ _{ webviewAddressAttr = newUrl, address = newUrl }

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
initialURL =
  MaybeT.runMaybeT url >>= Maybe.fromMaybe "" >>> prefixProtocol >>> pure
  where
    document = HTML.window >>= Window.document
    currentScript = MaybeT.MaybeT $ document >>= HTMLDocument.currentScript
    urlAttribute = Element.getAttribute "data-url" >>> MaybeT.MaybeT
    url = currentScript >>= HTMLScriptElement.toElement >>> urlAttribute

prefixProtocol :: String -> String
prefixProtocol str =
  Maybe.fromMaybe str $ ifM matchProtocol (pure str) (pure $ prefix str)
  where
    protocolRegex = Either.hush $ Regex.regex "^[a-z]+://" RegexFlags.noFlags
    matchProtocol = Regex.test <$> protocolRegex <@> str
    prefix = ("http://" <> _)

runCommand
  :: forall m
   . AffClass.MonadAff m
  => Config.Config
  -> Command.Command
  -> DSL m m Unit
runCommand config = Client.exec config >>> Aff.launchAff_ >>> Halogen.liftEffect

updateWindowTitle :: forall m. AffClass.MonadAff m => DSL m m Unit
updateWindowTitle = do
  state <- Halogen.get
  Halogen.liftEffect $ HTML.window >>= Window.document >>=
    flip DOM.setWindowTitle (state.title <> " - " <> state.address)

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

isFocused :: Node.Node -> Effect.Effect Boolean
isFocused node =
  HTML.window >>=
  Window.document >>=
  HTMLDocument.activeElement >>=
  Maybe.maybe (pure false) (HTMLElement.toNode >>> Node.isEqualNode node)

setZoom :: forall m. AffClass.MonadAff m => Number -> DSL m m Unit
setZoom zoom' = do
  Halogen.modify_ _{ zoomFactor = zoom' }
  void $ withWebviewElement $
    flip HTMLWebviewElement.setZoomFactor zoom' >>> Halogen.liftEffect

newZoom :: ZoomDirection.ZoomDirection -> Number -> Number
newZoom (ZoomDirection.In step) = (_ + (zoomStep * Int.toNumber step))
newZoom (ZoomDirection.Out step) = (_ - (zoomStep * Int.toNumber step))
newZoom ZoomDirection.Reset = const $ 1.0
