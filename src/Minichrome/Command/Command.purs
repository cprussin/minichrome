module Minichrome.Command.Command
  ( Command(..)
  , read
  , help
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String

import Minichrome.Command.InputMode as InputMode
import Minichrome.Command.Direction as Direction
import Minichrome.Command.YankTarget as YankTarget
import Minichrome.Command.ZoomDirection as ZoomDirection

data Command
  = CancelSearch
  | CloseSearch
  | Ex
  | LeaveEx
  | Navigate Int
  | Noop
  | OpenDevTools
  | Scroll Direction.Direction
  | Search String
  | SearchForward
  | SearchBack
  | SetMode InputMode.Mode
  | StartSearch
  | Yank YankTarget.YankTarget
  | Zoom ZoomDirection.ZoomDirection

instance showCommand :: Show Command where
  show CancelSearch = "cancel-search"
  show CloseSearch = "close-search"
  show Ex = "ex"
  show LeaveEx = "leave-ex"
  show (Navigate count)
    | count > 0 = "forward " <> show count
    | count < 0 = "back " <> show (-count)
    | otherwise = "noop"
  show Noop = "noop"
  show OpenDevTools = "dev-tools"
  show (Scroll direction) = "scroll " <> show direction
  show (Search str) = "search " <> str
  show SearchForward = "search-forward"
  show SearchBack = "search-back"
  show (SetMode mode) = "set-mode " <> show mode
  show StartSearch = "start-search"
  show (Yank target) = "yank " <> show target
  show (Zoom direction) = "zoom " <> show direction

help :: String
help = String.joinWith "\n"
  [ "Commands:"
  , "  cancel-search        clear search"
  , "  close-search         close search input"
  , "  back (<steps>)       go back, optionally by the number of steps"
  , "  dev-tools            open developer tools"
  , "  ex                   open ex input"
  , "  forward (<steps>)    go forward, optionally by the number of steps"
  , "  leave-ex             close ex input"
  , "  noop                 do nothing"
  , "  scroll <direction>   scroll in the page"
  , "  search <term>        search for the given term in the page"
  , "  search-forward       search for the next result using the last term"
  , "  search-back          search for the previous hit using the last term"
  , "  set-mode <mode>      switch to the given mode"
  , "  start-search         open search input"
  , "  yank <yank-target>   yank the specified target"
  , "  zoom <direction>     adjust the zoom level"
  , ""
  , Direction.help
  , ""
  , InputMode.help
  , ""
  , YankTarget.help
  , ""
  , ZoomDirection.help
  ]

read :: String -> Either.Either String Command
read = String.split (String.Pattern " ") >>> readTokens

readTokens :: Array String -> Either.Either String Command
readTokens tokens = Either.note "No command" (tokens !! 0) >>= case _ of
  "noop" -> pure Noop
  "set-mode" -> commandNote tokens (tokens !! 1) >>= InputMode.read <#> SetMode
  "scroll" -> tokenTail tokens >>= Direction.readTokens <#> Scroll
  "zoom" -> tokenTail tokens >>= ZoomDirection.readTokens <#> Zoom
  "dev-tools" -> pure OpenDevTools
  "forward" -> tokenTail tokens >>= case _ of
    [ count ] -> Navigate <$> parseNavigationStep count
    [] -> pure $ Navigate 1
    _ -> Either.Left "Usage: forward (<count>)"
  "back" -> tokenTail tokens >>= case _ of
    [ count ] -> Navigate <$> negate <$> parseNavigationStep count
    [] -> pure $ Navigate (-1)
    _ -> Either.Left "Usage: back (<count>)"
  "yank" -> commandNote tokens (tokens !! 1) >>= YankTarget.read <#> Yank
  "ex" -> pure Ex
  "leave-ex" -> pure LeaveEx
  "start-search" -> pure StartSearch
  "cancel-search" -> pure CancelSearch
  "close-search" -> pure CloseSearch
  "search-forward" -> pure SearchForward
  "search-back" -> pure SearchBack
  "search" -> tokenTail tokens <#> String.joinWith " " >>> Search
  badCmd -> Either.Left $ invalidCommandMessage badCmd

tokenTail :: Array String -> Either.Either String (Array String)
tokenTail tokens = commandNote tokens (Array.tail tokens)

commandNote :: forall t. Array String -> Maybe.Maybe t -> Either.Either String t
commandNote = String.joinWith " " >>> invalidCommandMessage >>> Either.note

invalidCommandMessage :: String -> String
invalidCommandMessage = ("Invalid command: " <> _)

parseNavigationStep :: String -> Either.Either String Int
parseNavigationStep step =
  Either.note ("Invalid navigation step: " <> step) $ Int.fromString step
