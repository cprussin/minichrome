module Minichrome.Server
  ( start
  ) where

import Prelude

import Effect as Effect
import Effect.Class as EffectClass
import Effect.Console as Console
import HTTPure as HTTPure
import HTTPure ((!@))
import Node.Electron.App as App

import Minichrome.Config as Config
import Minichrome.Window as Window

-- | Handle HTTP requests to open a new window to a URL given by the HTTP body.
browse :: Config.Config -> HTTPure.Request -> HTTPure.ResponseM
browse config { body } = do
  EffectClass.liftEffect $ Console.log $ "Opening new window to " <> body
  EffectClass.liftEffect $ Window.open config body
  HTTPure.ok $ "Opening new window to " <> body

-- | Route inbound HTTP requests to the appropriate method.
router :: Config.Config -> HTTPure.Request -> HTTPure.ResponseM
router config request@{ method, path }
  | method == HTTPure.Post && path !@ 0 == "browse" = browse config request
  | otherwise = do
    EffectClass.liftEffect $ Console.log $ show request
    HTTPure.notFound

-- | Start the Electron app.
startElectron :: Effect.Effect Unit -> Effect.Effect Unit
startElectron afterStart =
  App.onReady do
    App.onWindowAllClosed $ pure unit
    afterStart

-- | Start the Electron app and HTTP server.
start :: Config.Config -> HTTPure.ServerM
start config = startElectron $ HTTPure.serve config.port (router config) do
  Console.log $ "Minichrome server started!"
