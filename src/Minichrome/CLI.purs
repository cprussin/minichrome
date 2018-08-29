module Minichrome.CLI
  ( minichrome
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Maybe as Maybe
import Data.String as String
import Effect as Effect
import Node.Process as Process

import Minichrome.Client as Client
import Minichrome.Config as Config
import Minichrome.Server as Server

-- | True if the argument array starts with a call to the `electron` executable
-- | instead of the minichrome one.
isDefaultElectronApp :: Array String -> Boolean
isDefaultElectronApp argv = (firstArg >>= lastSegment) == Maybe.Just "electron"
  where
    firstArg = argv !! 0
    lastSegment = String.split (String.Pattern "/") >>> Array.last

-- | If using the default `electron` executable, the second argument will be the
-- | path to the electron app, which we don't care about.  Otherwise, we do care
-- | about the second argument.  This function returns the number of prefix
-- | arguments that we don't care about.
cliPrefixLength :: Array String -> Int
cliPrefixLength argv = if isDefaultElectronApp argv then 2 else 1

-- | If there are no non-prefix arguments, we should run the server.  Otherwise,
-- | we should run the client.
shouldRunServer :: Array String -> Boolean
shouldRunServer argv = Array.length argv == cliPrefixLength argv

-- | Return the interesting (non-prefix) arguments.
getClientArgs :: Array String -> Array String
getClientArgs argv = Array.drop (cliPrefixLength argv) argv

-- | The entry point: takes a config `Record` and runs either the server or the
-- | client, depending on the CLI arguments.
minichrome :: Config.Config -> Effect.Effect Unit
minichrome config = do
  argv <- Process.argv
  if shouldRunServer argv
     then Server.start config
     else Client.run config $ getClientArgs argv
