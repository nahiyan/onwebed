module Document (Document, toXmlElementTree) where

import Xml as Xml
import Tree as Tree
import Data.Argonaut.Parser as Parser
import Data.Argonaut.Decode as JsonDecode
import Data.Either (Either(..))
import Prelude

type Document
  = { name :: String, body :: Array Xml.Element }

toXmlElementTree :: String -> String -> Tree.Tree Xml.Element
toXmlElementTree sourceDirectory content =
  let
    empty = Tree.singleton Xml.Root
  in
    case Parser.jsonParser content of
      Left left -> empty
      Right json -> case json # JsonDecode.decodeJson of
        Left _ -> empty
        Right tree -> tree
