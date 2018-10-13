module Minichrome.Temp.NodeModule
  ( NodeModule
  , main
  ) where

import Node.Path as Path

type NodeModule = Record
  ( id :: String
  , filename :: Path.FilePath
  , loaded :: Boolean
  , paths :: Array Path.FilePath
  )

foreign import main :: NodeModule
