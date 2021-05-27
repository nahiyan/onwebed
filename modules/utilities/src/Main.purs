module Main (isPublic, isDocument) where

import Prelude
import Data.String as String
import Node.Path as Path

isPublic :: String -> Boolean
isPublic name = (isDocument name) && ((String.take 1 name) /= "_")

isDocument :: String -> Boolean
isDocument name = Path.extname name == ".od"
