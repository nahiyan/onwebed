module Document.Body.Fills (Fills, fromBody) where

import Tree as Tree
import Tree.Zipper as Zipper
import Xml as Xml
import Data.Map as Map
import Foreign.Object as FObject
import Data.Maybe as Maybe
import Prelude

type Fills
  = Map.Map String (Array (Tree.Tree Xml.Element))

fromBody :: Tree.Tree Xml.Element -> Fills
fromBody body = fromBody' (body # Zipper.fromTree) Map.empty

fromBody' :: Zipper.Zipper Xml.Element -> Fills -> Fills
fromBody' zipper collection =
  let
    newCollection = case zipper # Zipper.label of
      Xml.Element { name, attributes } ->
        if name == "fill" then case attributes # FObject.lookup "id" of
          Maybe.Nothing -> collection
          Maybe.Just id -> collection # Map.insert id (zipper # Zipper.children)
        else
          collection
      _ -> collection
  in
    case zipper # Zipper.forward of
      Maybe.Just newZipper -> fromBody' newZipper newCollection
      Maybe.Nothing -> newCollection
