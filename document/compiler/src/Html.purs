module Html (saveFiles, fromDocumentContent) where

import Prelude (Unit, pure, unit, bind, (#))
import Effect (Effect)
import Effect.Console (log)
import Data.Array as Array
import Node.FS.Sync as FSSync
import Node.Encoding as Encoding
import Data.Semigroup ((<>))

saveFiles :: Array String -> Array (Effect String) -> Effect Unit
saveFiles filePaths readFiles =
  Array.zipWith
    ( \filePath readFile ->
        bind readFile \content ->
          (log ("Written " <> filePath)) <> FSSync.writeTextFile Encoding.UTF8 filePath content
    )
    filePaths
    readFiles
    # Array.foldl (<>) (pure unit)

fromDocumentContent :: String -> String -> String
fromDocumentContent sourceDirectory content = ""
