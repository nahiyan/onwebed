module Main (compileFromDirectory) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync as FSSync
import Node.Encoding as Encoding
import Node.Path as Path
import Data.Array as Array
import Html as Html

foreign import isPublic :: String -> Boolean

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
          let
            compile =
              bind (FSSync.readdir sourceDirectory) \items ->
                let
                  publicDocumentNames = Array.filter isPublic items

                  inputFilePaths =
                    publicDocumentNames
                      # map (\fileName -> Path.concat [ sourceDirectory, fileName ])

                  outputFilePaths =
                    Array.filter isPublic items
                      # map (\fileName -> Path.concat [ destinationDirectory, fileName <> ".html" ])
                in
                  inputFilePaths
                    # map
                        ( \inputFilePath ->
                            bind (FSSync.readTextFile Encoding.UTF8 inputFilePath) \content ->
                              Html.fromDocumentContent sourceDirectory content # pure
                        )
                    # Html.saveFiles outputFilePaths
          in
            prepareDestinationDirectory <> compile
