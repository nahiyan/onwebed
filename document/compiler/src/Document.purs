module Document (Document, toXmlElementTree) where

import Xml.Element as XmlElement
import Tree as Tree
import Data.Argonaut.Parser as Parser
import Data.Argonaut.Decode as JsonDecode
import Data.Either (Either(..))
import Prelude

type Document
  = { name :: String, body :: Array XmlElement.Element }

toXmlElementTree :: String -> String -> Tree.Tree XmlElement.Element
toXmlElementTree sourceDirectory content =
  let
    empty = Tree.singleton XmlElement.Root
  in
    case Parser.jsonParser content of
      Left left -> empty
      Right json -> case json # JsonDecode.decodeJson of
        Left _ -> empty
        Right tree -> tree
