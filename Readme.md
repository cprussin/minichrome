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
yarn build
```

## Configuring

TODO

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
