module Main (compile, compileFromDirectory) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync as FSSync
import Node.Encoding as Encoding
import Node.Path as Path
import Data.Array as Array
import Html as Html

foreign import isPublic :: String -> Boolean

compile :: String -> String -> String -> Effect Unit
compile name sourceDirectory destinationDirectory =
  if isPublic name then
    readPath
      ( \content ->
          let
            compile_ = Html.saveFiles [ (outputFileName destinationDirectory name) ] [ pure $ Html.fromDocumentContent sourceDirectory content ]
          in
            prepareDestinationDirectory destinationDirectory <> compile_
      )
      (Path.concat [ sourceDirectory, name ])
  else
    log ("Error: Document " <> name <> " isn't public.")

prepareDestinationDirectory :: String -> Effect Unit
prepareDestinationDirectory destinationDirectory = do
  destinationDirectoryExists <- FSSync.exists destinationDirectory
  if not destinationDirectoryExists then
    FSSync.mkdir destinationDirectory
  else
    pure unit

readSourceDirectory :: (Array String -> Effect Unit) -> String -> Effect Unit
readSourceDirectory f sourceDirectory = do
  sourceDirectoryExists <- FSSync.exists sourceDirectory
  if not sourceDirectoryExists then
    log ("Error: Source directory " <> sourceDirectory <> " doesn't exist!")
  else do
    items <- FSSync.readdir sourceDirectory
    f items

readPath :: (String -> Effect Unit) -> String -> Effect Unit
readPath f path = do
  pathExists <- FSSync.exists path
  if not pathExists then
    log ("Error: Path " <> path <> " doesn't exist!")
  else do
    content <- FSSync.readTextFile Encoding.UTF8 path
    f content

outputFileName :: String -> String -> String
outputFileName destinationDirectory inputFileName = Path.concat [ destinationDirectory, (Path.extname inputFileName # Path.basenameWithoutExt inputFileName) <> ".html" ]

compileFromDirectory :: String -> String -> Effect Unit
compileFromDirectory sourceDirectory destinationDirectory = do
  readSourceDirectory
    ( \items ->
        let
          compile_ =
            let
              publicDocumentNames = items # Array.filter isPublic

              outputFilePaths =
                publicDocumentNames
                  # map (outputFileName destinationDirectory)
            in
              publicDocumentNames
                # map (\fileName -> Path.concat [ sourceDirectory, fileName ])
                # map
                    ( \inputFilePath -> do
                        content <- FSSync.readTextFile Encoding.UTF8 inputFilePath
                        pure $ Html.fromDocumentContent sourceDirectory content
                    )
                # Html.saveFiles outputFilePaths
        in
          prepareDestinationDirectory destinationDirectory <> compile_
    )
    sourceDirectory
