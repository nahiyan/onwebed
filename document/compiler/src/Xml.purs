module Xml (Element(..), Attributes, emptyElement, attributesFromString, toJson, fromDocumentName) where

import Prelude
import Foreign.Object as FObject
import Node.FS.Sync as FSSync
import Node.Path as Path
import Node.Encoding as Encoding
import Effect as Effect

type Attributes
  = FObject.Object String

foreign import attributesFromString :: String -> Attributes

foreign import toJson :: String -> String

data Element
  = Element { name :: String, attributes :: Attributes }
  | Bone { descriptor :: String }
  | Flesh { targets :: String, content :: String }
  | Text String
  | Document
  | Head
  | Body
  | Root
  | Blank

instance showElement :: Show Element where
  show (Element { name }) = "Element: name = " <> name
  show (Bone { descriptor }) = "Bone: descriptor = " <> descriptor
  show (Flesh { targets, content }) = "Flesh: targets = " <> targets <> ", content = " <> content
  show (Text text) = "Text: " <> text
  show Document = "Document"
  show Head = "Head"
  show Body = "Body"
  show Root = "Root"
  show Blank = "Blank"

instance compareXmlElements :: Eq Element where
  eq (Element a) (Element b) = a.name == b.name && a.attributes == b.attributes
  eq (Bone a) (Bone b) = a.descriptor == b.descriptor
  eq (Flesh a) (Flesh b) = a.targets == b.targets && a.content == b.content
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
  bind (FSSync.readTextFile Encoding.UTF8 (Path.concat [ sourceDirectory, name <> ".od" ])) \content ->
    pure content
