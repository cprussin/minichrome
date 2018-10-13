module Minichrome.CLI.Client
  ( browse
  , exec
  , run
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Either as Either
import Data.Maybe as Maybe
import Data.Options as Options
import Data.Options ((:=))
import Data.String as String
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Effect.Console as Console
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.HTTP.Client as HTTPClient
import Node.Process as Process
import Node.Stream as Stream

import Minichrome.Command.Command as Command
import Minichrome.Config as Config

-- | Show a helpful client CLI usage message.
showUsage :: Aff.Aff Unit
showUsage = EffectClass.liftEffect $ Console.log $ help

-- | The CLI usage message.
help :: String
help = String.joinWith "\n"
  [ ""
  , "Usage:"
  , "  minichrome                  start a new minichrome server"
  , "  minichrome browse <url>     open a new window with the given url"
  , "  minichrome exec <command>   run the command in the current window"
  , "  minichrome help             show this helpful help"
  , ""
  , prefixLines "  " Command.help
  , ""
  ]

prefixLines :: String -> String -> String
prefixLines prefix =
  String.split (String.Pattern "\n") >>>
  map (prefix <> _) >>>
  String.joinWith "\n"

-- | Given a `Readable` stream, return an `Aff` containing the string contents
-- | once it's all read.
streamToString :: Stream.Readable () -> Aff.Aff String
streamToString stream = Aff.makeAff \done -> do
  buf <- Ref.new ""
  Stream.onDataString stream Encoding.UTF8 \str ->
    void $ Ref.modify (str <> _) buf
  Stream.onEnd stream $ Ref.read buf >>= Either.Right >>> done
  pure Aff.nonCanceler

-- | Make a request with a body, but do it as an `Aff` instead of `Effect`.
affRequest ::
  Options.Options HTTPClient.RequestOptions ->
  String ->
  Aff.Aff HTTPClient.Response
affRequest options body = Aff.makeAff $ \done -> do
  req <- HTTPClient.request options $ Either.Right >>> done
  let stream = HTTPClient.requestAsStream req
  void $ Stream.writeString stream Encoding.UTF8 body $ pure unit
  Stream.end stream $ pure unit
  pure Aff.nonCanceler

-- | Make a request to the Minichrome server and print the server response.
request :: String -> Config.Config -> String -> Aff.Aff Unit
request path config = affRequest options >=> responseString >=> affLog
  where
    affLog = Console.log >>> EffectClass.liftEffect
    responseString = HTTPClient.responseAsStream >>> streamToString
    options =
      HTTPClient.protocol := "http:" <>
      HTTPClient.method   := "POST" <>
      HTTPClient.hostname := "localhost" <>
      HTTPClient.port     := config.port <>
      HTTPClient.path     := path

-- | Send a request to the server to open a new window pointing to the given
-- | URL.
browse :: Config.Config -> String -> Aff.Aff Unit
browse = request "/browse"

-- | Send a request to the server to run some `Command` in the current window.
exec :: Config.Config -> Command.Command -> Aff.Aff Unit
exec config = show >>> request "/exec" config

-- | Try to match the string to a command.  If there is a match, run it in the
-- | current window.  Otherwise, show a help message.
execS :: Config.Config -> String -> Aff.Aff Unit
execS config = Command.read >>> Either.either (const showUsage) (exec config)

-- | Run the client-side CLI on the given array of arguments.
run :: Config.Config -> Array String -> Effect.Effect Unit
run config args = void $ Aff.runAff (const $ pure unit) do
  case args !! 0 of
    (Maybe.Just "browse") -> run' browse $ args !! 1
    (Maybe.Just "exec") -> run' execS $ String.joinWith " " <$> Array.tail args
    _ -> showUsage
  EffectClass.liftEffect $ Process.exit 0
  where
    run' cmd = Maybe.maybe showUsage (cmd config)
