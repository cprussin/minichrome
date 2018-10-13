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
  | Go String
  | HardRefresh
  | LeaveEx
  | Navigate Int
  | Noop
  | OpenDevTools
  | Refresh
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
  show (Go url) = "go " <> url
  show HardRefresh = "hard-refresh"
  show LeaveEx = "leave-ex"
  show (Navigate count)
    | count > 0 = "forward " <> show count
    | count < 0 = "back " <> show (-count)
    | otherwise = "refresh"
  show Noop = "noop"
  show OpenDevTools = "dev-tools"
  show Refresh = "refresh"
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
  , "  e <url>              go to the given URL (synonym for `go`)"
  , "  ex                   open ex input"
  , "  forward (<steps>)    go forward, optionally by the number of steps"
  , "  go <url>             go to the given URL"
  , "  hard-refresh         refresh the page, ignoring the cache"
  , "  leave-ex             close ex input"
  , "  noop                 do nothing"
  , "  refresh              refresh the page"
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
  "back" -> tokenTail tokens >>= case _ of
    [ count ] -> Navigate <$> negate <$> parseNavigationStep count
    [] -> pure $ Navigate (-1)
    _ -> Either.Left "Usage: back (<count>)"
  "cancel-search" -> pure CancelSearch
  "close-search" -> pure CloseSearch
  "dev-tools" -> pure OpenDevTools
  "e" -> commandNote tokens (tokens !! 1) <#> Go
  "ex" -> pure Ex
  "forward" -> tokenTail tokens >>= case _ of
    [ count ] -> Navigate <$> parseNavigationStep count
    [] -> pure $ Navigate 1
    _ -> Either.Left "Usage: forward (<count>)"
  "go" -> commandNote tokens (tokens !! 1) <#> Go
  "hard-refresh" -> pure HardRefresh
  "leave-ex" -> pure LeaveEx
  "noop" -> pure Noop
  "refresh" -> pure Refresh
  "scroll" -> tokenTail tokens >>= Direction.readTokens <#> Scroll
  "search" -> tokenTail tokens <#> String.joinWith " " >>> Search
  "search-back" -> pure SearchBack
  "search-forward" -> pure SearchForward
  "set-mode" -> commandNote tokens (tokens !! 1) >>= InputMode.read <#> SetMode
  "start-search" -> pure StartSearch
  "yank" -> commandNote tokens (tokens !! 1) >>= YankTarget.read <#> Yank
  "zoom" -> tokenTail tokens >>= ZoomDirection.readTokens <#> Zoom
  badCmd -> Either.Left $ invalidCommandMessage badCmd

tokenTail :: Array String -> Either.Either String (Array String)
tokenTail tokens = commandNote tokens $ Array.tail tokens

commandNote :: forall t. Array String -> Maybe.Maybe t -> Either.Either String t
commandNote = String.joinWith " " >>> invalidCommandMessage >>> Either.note

invalidCommandMessage :: String -> String
invalidCommandMessage = ("Invalid command: " <> _)

parseNavigationStep :: String -> Either.Either String Int
parseNavigationStep step =
  Either.note ("Invalid navigation step: " <> step) $ Int.fromString step
