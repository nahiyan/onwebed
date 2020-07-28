module Document.Body.Fills (Fills, fromBody, FillType(..)) where

import Tree as Tree
import Tree.Zipper as Zipper
import Xml as Xml
import Data.Map as Map
import Foreign.Object as FObject
import Data.Maybe as Maybe
import Prelude
import Data.Tuple as Tuple

data FillType
  = Set
  | Append
  | Prepend

instance showFillType :: Show FillType where
  show Set = "Set"
  show Append = "Append"
  show Prepend = "Prepend"

instance eqFillType :: Eq FillType where
  eq Set Set = true
  eq Append Append = true
  eq Prepend Prepend = true
  eq _ _ = false

type Fills
  = Map.Map String (Tuple.Tuple FillType (Array (Tree.Tree Xml.Element)))

fromBody :: Tree.Tree Xml.Element -> Fills
fromBody body = fromBody' (body # Zipper.fromTree) Map.empty

fromBody' :: Zipper.Zipper Xml.Element -> Fills -> Fills
fromBody' zipper collection =
  let
    newCollection = case zipper # Zipper.label of
      Xml.Element { name, attributes } ->
        if name == "fill" then case attributes # FObject.lookup "id" of
          Maybe.Nothing -> collection
          Maybe.Just id ->
            let
              fillType =
                attributes # FObject.lookup "class"
                  # Maybe.maybe Set
                      ( \fillType_ -> case fillType_ of
                          "prepend" -> Prepend
                          "append" -> Append
                          _ -> Set
                      )
            in
              collection # Map.insert id (Tuple.Tuple fillType (zipper # Zipper.children))
        else
          collection
      _ -> collection
  in
    case zipper # Zipper.forward of
      Maybe.Just newZipper -> fromBody' newZipper newCollection
      Maybe.Nothing -> newCollection
