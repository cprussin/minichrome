module Minichrome.CLI.Client
  ( run
  ) where

import Prelude

import Data.Array ((!!))
import Data.Either as Either
import Data.Maybe as Maybe
import Data.Options as Options
import Data.Options ((:=))
import Effect as Effect
import Effect.Aff as Aff
import Effect.Class as EffectClass
import Effect.Console as Console
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.HTTP.Client as HTTPClient
import Node.Process as Process
import Node.Stream as Stream

import Minichrome.Config as Config

-- | Show a helpful client CLI usage message.
showUsage :: Effect.Effect Unit
showUsage = do
  Console.log $ "Usage:"
  Console.log $ "  minichrome browse <url>"

-- | Given a `Readable` stream, return an `Aff` containing the string contents
-- | once it's all read.
streamToString :: Stream.Readable () -> Aff.Aff String
streamToString stream = Aff.makeAff \done -> do
  buf <- Ref.new ""
  Stream.onDataString stream Encoding.UTF8 \str ->
    void $ Ref.modify ((<>) str) buf
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
  _ <- Stream.writeString stream Encoding.UTF8 body $ pure unit
  Stream.end stream $ pure unit
  pure Aff.nonCanceler

-- | Make a request to the Minichrome server and print the server response.
request :: String -> Config.Config -> String -> Aff.Aff Unit
request path config body = affRequest options body >>= responseString >>= affLog
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

-- | Run the client-side CLI on the given array of arguments.
run :: Config.Config -> Array String -> Effect.Effect Unit
run config args = void $ Aff.runAff (\_ -> pure unit) do
  case args !! 0 of
    (Maybe.Just "browse") -> browse config $ Maybe.fromMaybe "" $ args !! 1
    _ -> EffectClass.liftEffect showUsage
  EffectClass.liftEffect $ Process.exit 0
