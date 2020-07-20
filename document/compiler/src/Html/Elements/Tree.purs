module Html.Elements.Tree (fromBoneDescriptorElements) where

import Bone.Descriptor as Descriptor
import Bone.Descriptor.Element.Targets as Targets
import Tree as Tree
import Tree.Zipper as Zipper
import Xml as Xml
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Map as Map
import Data.String as String
import Foreign.Object as FObject
import Prelude

fromBoneDescriptorElements :: Array Descriptor.Element -> Targets.Targets -> Maybe.Maybe (Tree.Tree Xml.Element) -> Tree.Tree Xml.Element -> String -> Tree.Tree Xml.Element
fromBoneDescriptorElements elements targets endTree tree sourceDirectory =
  Array.foldl
    ( \acc element ->
        let
          targetedText = targets # Map.lookup element.id

          attributes =
            Xml.attributesFromString element.attributes
              # (if element.xId # String.null then identity else FObject.insert "id" element.xId)
              # (if element.xClass # String.null then identity else FObject.insert "class" element.xClass)

          newTreeChildren = case targetedText of
            Maybe.Nothing ->
              if element.hasClosingTag then
                [ Tree.singleton (Xml.Text "") ]
              else
                []
            Maybe.Just text -> [ Tree.singleton (Xml.Text text) ]

          newTree = Tree.tree (Xml.Element { name: element.name, attributes: attributes }) newTreeChildren
        in
          acc # Zipper.fromTree
            # Zipper.lastDescendant
            # Zipper.mapTree (\(Tree.Tree label children) -> Tree.tree label (Array.snoc children newTree))
            # Zipper.toTree
    )
    tree
    elements
    # Zipper.fromTree
    # Zipper.lastDescendant
    # Zipper.mapTree
        ( \(Tree.Tree label children) ->
            Tree.tree label
              ( case endTree of
                  Maybe.Nothing -> children
                  Maybe.Just justEndTree -> (Array.snoc children justEndTree)
              )
        )
    # Zipper.toTree
