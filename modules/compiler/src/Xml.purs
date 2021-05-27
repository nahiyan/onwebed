module Xml (Element(..), Attributes, emptyElement, attributesFromString, toJson, fromDocumentName, mergeAttributes) where

import Prelude
import Foreign.Object as FObject
import Node.FS.Sync as FSSync
import Node.Path as Path
import Node.Encoding as Encoding
import Effect as Effect
import Data.Maybe as Maybe

type Attributes
  = FObject.Object String

foreign import attributesFromString :: String -> Attributes

foreign import toJson :: String -> String

data Element
  = Element { name :: String, attributes :: Attributes }
  | Bone { descriptor :: String }
  | Flesh { for :: String, content :: Maybe.Maybe String, attributes :: Maybe.Maybe Attributes }
  | Text String
  | Document
  | Head
  | Body
  | Root
  | Blank

instance showElement :: Show Element where
  show (Element { name }) = "Element: name = " <> name
  show (Bone { descriptor }) = "Bone: descriptor = " <> descriptor
  show (Flesh { for, content, attributes }) = "Flesh: targets = " <> for <> ", attributes = " <> (attributes # show) <> ", content = " <> (content # show)
  show (Text text) = "Text: " <> text
  show Document = "Document"
  show Head = "Head"
  show Body = "Body"
  show Root = "Root"
  show Blank = "Blank"

instance compareXmlElements :: Eq Element where
  eq (Element a) (Element b) = a.name == b.name && a.attributes == b.attributes
  eq (Bone a) (Bone b) = a.descriptor == b.descriptor
  eq (Flesh a) (Flesh b) = a.for == b.for && a.content == b.content
  eq (Text a) (Text b) = a == b
  eq Document Document = true
  eq Head Head = true
  eq Body Body = true
  eq Root Root = true
  eq Blank Blank = true
  eq _ _ = false

emptyElement :: Element
emptyElement = Element { name: "", attributes: FObject.empty }

fromDocumentName :: String -> String -> Effect.Effect String
fromDocumentName name sourceDirectory =
  let
    publicDocumentPath = (Path.concat [ sourceDirectory, name <> ".od" ])

    privateDocumentPath = (Path.concat [ sourceDirectory, "_" <> name <> ".od" ])
  in
    do
      publicDocumentExists <- FSSync.exists publicDocumentPath
      privateDocumentExists <- FSSync.exists privateDocumentPath
      if publicDocumentExists then do
        content <- FSSync.readTextFile Encoding.UTF8 publicDocumentPath
        pure content
      else
        if privateDocumentExists then do
          content <- FSSync.readTextFile Encoding.UTF8 privateDocumentPath
          pure content
        else
          pure $ "Document " <> name <> " doesn't exist."

mergeAttributes :: Attributes -> Attributes -> Attributes
mergeAttributes a b =
  FObject.fold
    ( \acc key value ->
        acc
          # FObject.alter
              ( Maybe.maybe (Maybe.Just value)
                  (\bValue -> Maybe.Just $ value <> bValue)
              )
              key
    )
    b
    a
