module Document.Body.Fills (Fills, collect) where

import Tree as Tree
import Tree.Zipper as Zipper
import Xml as Xml
import Data.Map as Map
import Foreign.Object as FObject
import Data.Maybe as Maybe
import Prelude

type Fills
  = Map.Map String (Array (Tree.Tree Xml.Element))

collect :: Tree.Tree Xml.Element -> Fills
collect body = collect' (body # Zipper.fromTree) Map.empty

collect' :: Zipper.Zipper Xml.Element -> Fills -> Fills
collect' zipper collection =
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
      Maybe.Just newZipper -> collect' newZipper newCollection
      Maybe.Nothing -> newCollection
