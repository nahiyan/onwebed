module Main (compileFromDirectory) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync as FSSync

compileFromDirectory :: String -> String -> Effect Unit
compileFromDirectory sourceDirectory destinationDirectory =
  bind (FSSync.exists destinationDirectory) \destinationDirectoryExists ->
    let
      prepareDestinationDirectory =
        if not destinationDirectoryExists then
          FSSync.mkdir destinationDirectory
        else
          pure unit
    in
      bind (FSSync.exists sourceDirectory) \sourceDirectoryExists ->
        if not sourceDirectoryExists then
          log "Error: Source directory doesn't exist!"
        else
          prepareDestinationDirectory <> log "Compiling"
