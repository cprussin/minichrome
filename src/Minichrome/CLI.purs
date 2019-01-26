module Minichrome.CLI
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Maybe as Maybe
import Data.String as String
import Effect as Effect
import Node.Process as Process

import Minichrome.Config as Config
import Minichrome.CLI.Client as Client
import Minichrome.CLI.Server as Server

-- | True if the argument array starts with a call to the `electron` executable
-- | instead of the minichrome one.
isDefaultElectronApp :: Array String -> Boolean
isDefaultElectronApp argv =
  Maybe.maybe false isElectronExecutable $ firstArg >>= lastSegment
  where
    firstArg = argv !! 0
    lastSegment = String.split (String.Pattern "/") >>> Array.last
    isElectronExecutable = String.contains (String.Pattern "electron")

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

-- | Take a config `Record` and run either the server or the client, depending
-- | on the CLI arguments.
runCLI :: Config.Config -> Effect.Effect Unit
runCLI config = do
  argv <- Process.argv
  if shouldRunServer argv
     then Server.start config
     else Client.run config $ getClientArgs argv

-- | The entry point to the CLI.
main :: Effect.Effect Unit
main = runCLI Config.defaultConfig
