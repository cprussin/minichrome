module Minichrome.Page.FocusCommands
  ( blur
  , nextForMode
  , previousForMode
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Maybe as Maybe
import Effect as Effect
import Web.HTML.HTMLElement as HTMLElement

import Minichrome.Command.InputMode as InputMode
import Minichrome.IPC.PageToUI as IPCUp
import Minichrome.Page.Util as Util
import Minichrome.Temp.Foldable as TFoldable

blur :: Effect.Effect Unit
blur = Util.getActiveElement >>= Maybe.maybe mempty HTMLElement.blur

nextForMode :: InputMode.Mode -> Effect.Effect Unit
nextForMode = focusForMode 1 $ flip Array.index 0

previousForMode :: InputMode.Mode -> Effect.Effect Unit
previousForMode = focusForMode (-1) Array.last

focusForMode
  :: Int
  -> (Array HTMLElement.HTMLElement -> Maybe.Maybe HTMLElement.HTMLElement)
  -> InputMode.Mode
  -> Effect.Effect Unit
focusForMode offsetFromCurrent contingent mode =
  Util.withAllVisibleElementsForMode mode \elements ->
    if Array.null elements
      then showMessage $ "No elements for mode " <> show mode
      else doFocus elements
  where
    doFocus elements =
      Util.getActiveElement >>=
      Maybe.maybe (focus $ contingent elements) \current ->
        ifM (isOnly elements current)
          (showMessage $ "No other elements for mode " <> show mode)
          (getElement elements current >>= focus)
    getElement elems elem = do
      index <- TFoldable.findWithIndexM (const $ Util.isEqualElement elem) elems
      pure $ Maybe.maybe (contingent elems) pure $
        index >>= _.index >>> (_ + offsetFromCurrent) >>> Array.index elems

showMessage :: String -> Effect.Effect Unit
showMessage = IPCUp.ShowMessage >>> IPCUp.send

focus :: Maybe.Maybe HTMLElement.HTMLElement -> Effect.Effect Unit
focus = Maybe.maybe mempty HTMLElement.focus

isOnly
  :: Array HTMLElement.HTMLElement
  -> HTMLElement.HTMLElement
  -> Effect.Effect Boolean
isOnly elements element =
  if Array.length elements == 1
    then Maybe.maybe (pure false) (Util.isEqualElement element) $ elements !! 0
    else pure false
