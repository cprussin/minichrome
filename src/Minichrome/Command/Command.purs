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
  = Ex
  | LeaveEx
  | Navigate Int
  | Noop
  | OpenDevTools
  | Scroll Direction.Direction
  | SetMode InputMode.Mode
  | Yank YankTarget.YankTarget
  | Zoom ZoomDirection.ZoomDirection

instance showCommand :: Show Command where
  show Ex = "ex"
  show LeaveEx = "leave-ex"
  show (Navigate count)
    | count > 0 = "forward " <> show count
    | count < 0 = "back " <> show (-count)
    | otherwise = "noop"
  show Noop = "noop"
  show OpenDevTools = "dev-tools"
  show (Scroll direction) = "scroll " <> show direction
  show (SetMode mode) = "set-mode " <> show mode
  show (Yank target) = "yank " <> show target
  show (Zoom direction) = "zoom " <> show direction

help :: String
help = String.joinWith "\n"
  [ "Commands:"
  , "  back (<steps>)       go back, optionally by the number of steps"
  , "  dev-tools            open developer tools"
  , "  ex                   open ex input"
  , "  forward (<steps>)    go forward, optionally by the number of steps"
  , "  leave-ex             close ex input"
  , "  noop                 do nothing"
  , "  scroll <direction>   scroll in the page"
  , "  set-mode <mode>      switch to the given mode"
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
  "scroll" ->
    commandNote tokens (Array.tail tokens) >>= Direction.readTokens <#> Scroll
  "zoom" ->
    commandNote tokens (Array.tail tokens) >>= ZoomDirection.readTokens <#> Zoom
  "dev-tools" -> pure OpenDevTools
  "forward" -> commandNote tokens (Array.tail tokens) >>= case _ of
    [ count ] -> Navigate <$> parseNavigationStep count
    [] -> pure $ Navigate 1
    _ -> Either.Left "Usage: forward (<count>)"
  "back" -> commandNote tokens (Array.tail tokens) >>= case _ of
    [ count ] -> Navigate <$> negate <$> parseNavigationStep count
    [] -> pure $ Navigate (-1)
    _ -> Either.Left "Usage: back (<count>)"
  "yank" -> commandNote tokens (tokens !! 1) >>= YankTarget.read <#> Yank
  "ex" -> pure Ex
  "leave-ex" -> pure LeaveEx
  badCmd -> Either.Left $ invalidCommandMessage badCmd

commandNote :: forall t. Array String -> Maybe.Maybe t -> Either.Either String t
commandNote = String.joinWith " " >>> invalidCommandMessage >>> Either.note

invalidCommandMessage :: String -> String
invalidCommandMessage = ("Invalid command: " <> _)

parseNavigationStep :: String -> Either.Either String Int
parseNavigationStep step =
  Either.note ("Invalid navigation step: " <> step) $ Int.fromString step
