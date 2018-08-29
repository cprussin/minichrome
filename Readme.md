# Minichrome

Minichrome is a minimalistic web browser, built especially with tiling window
managers in mind.  It explicitly does not have a tabbed UI and avoids most UI
chrome in favor of keyboard shortcuts.

Minichrome is built on [Electron](https://electronjs.org).

In order to stay fast and reasonable on resources, Minichrome is built as a
client-server architecture.  The server runs an HTTP interface and owns all
browser windows, and the client provides a simple CLI wrapper over the HTTP
calls to create and manipulate windows.  This means you'll only have one
instance of Electron running for all your Minichrome windows, rather than one
instance of Electron per window.

## Building

To build `Minichrome`, run:

```sh
bower install
yarn install
yarn build
```

## Configuring

Configuration is heavily inspired by [XMonad](http://xmonad.org).  To use the
default configuration, you don't need to do anything--Minichrome will run out of
the box that way.  To modify the configuration, you'll want to create a
PureScript entry point that looks something like this:

```purescript
module Main where

import Prelude

import Data.Tuple as Tuple
import Effect as Effect
import Minichrome.Actions as Actions
import Minichrome.CLI as CLI
import Minichrome.Config as Config

main :: Effect.Effect Unit
main = CLI.minichrome Config.defaultConfig
  { port = 12312
  , keybindings =
    [ Tuple.Tuple (Config.Shortcut "h" true) Actions.goBack
    , Tuple.Tuple (Config.Shortcut "l" true) Actions.goForward
    ]
  }
```

## CLI

To run the server, after building, run 

```sh
yarn start
```

To open a new window, run:

```sh
yarn start browse "http://xkcd.com"
```

## Status

This project is currently an early-stage work in progress.  If you have interest
in what's being built, feel free to contribute!  PRs, issues, and feature
requests are welcome.
