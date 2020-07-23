module Html.Elements.Tree (fromBoneDescriptorElements, fromDocumentContent) where

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
import Data.Argonaut.Decode as JsonDecode
import Data.Argonaut.Parser as Parser
import Data.Either as Either
import Document.Body.Fills as Fills
import Document.Body.Holes as Holes
import Effect as Effect
import Effect.Unsafe as EffectUnsafe

lastHtmlElementZipper :: Zipper.Zipper Xml.Element -> Zipper.Zipper Xml.Element
lastHtmlElementZipper zipper = lastHtmlElementZipper' (zipper # Zipper.lastDescendant)

lastHtmlElementZipper' :: Zipper.Zipper Xml.Element -> Zipper.Zipper Xml.Element
lastHtmlElementZipper' zipper = case zipper # Zipper.tree of
  Tree.Tree (Xml.Element _) children -> zipper
  _ -> case (zipper # Zipper.backward) of
    Maybe.Just newZipper -> lastHtmlElementZipper' newZipper
    Maybe.Nothing -> zipper # Zipper.root

fromBoneDescriptorElements :: Array Descriptor.Element -> Targets.Targets -> Array (Tree.Tree Xml.Element) -> String -> Tree.Tree Xml.Element
fromBoneDescriptorElements elements targets endChildren sourceDirectory =
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
              if element.hasClosingTag && element.name /= "fill" then
                [ Tree.singleton (Xml.Text "") ]
              else
                []
            Maybe.Just text -> [ Tree.singleton (Xml.Text text) ]

          newTree =
            if element.name /= "page" then
              Tree.tree (Xml.Element { name: element.name, attributes: attributes }) newTreeChildren
            else case attributes # FObject.lookup "id" of
              Maybe.Just id -> EffectUnsafe.unsafePerformEffect $ fromDocumentName (id <> ".od") sourceDirectory targets
              Maybe.Nothing -> Tree.singleton Xml.Blank
        in
          acc # Zipper.fromTree
            # lastHtmlElementZipper
            # Zipper.mapTree
                ( \(Tree.Tree label children) ->
                    Tree.tree label (Array.snoc children newTree)
                )
            # Zipper.toTree
    )
    (Tree.singleton Xml.Root)
    elements
    # Tree.children
    # Array.head
    # Maybe.fromMaybe (Tree.singleton Xml.Root)
    # Zipper.fromTree
    # Zipper.lastDescendant
    # Zipper.mapTree
        ( \(Tree.Tree label children) ->
            Tree.tree label (children <> endChildren)
        )
    # Zipper.toTree

fromDocumentContent :: String -> String -> Tree.Tree Xml.Element
fromDocumentContent content sourceDirectory = fromDocumentContent' content Targets.empty sourceDirectory true

fromDocumentContent' :: String -> Targets.Targets -> String -> Boolean -> Tree.Tree Xml.Element
fromDocumentContent' content targets sourceDirectory shouldFillHoles = case Xml.toJson content # Parser.jsonParser of
  Either.Right json -> case json # JsonDecode.decodeJson of
    Either.Right tree -> case tree # Zipper.fromTree # Zipper.findFromRoot (\element -> element == Xml.Body) of
      Maybe.Just bodyZipper ->
        let
          body = bodyZipper # Zipper.tree

          fleshItems =
            body
              # Tree.foldl
                  ( \element acc -> case element of
                      Xml.Flesh flesh -> Array.snoc acc element
                      _ -> acc
                  )
                  []

          combinedTargets = Targets.merge targets (Targets.fromFleshItems fleshItems)

          bodyRestructured =
            body
              # Tree.restructure identity
                  ( \element children -> case element of
                      Xml.Bone bone -> fromBoneDescriptorElements (Descriptor.toElements bone.descriptor) combinedTargets children sourceDirectory
                      Xml.Body -> Tree.tree Xml.Root children
                      _ -> Tree.singleton Xml.Blank
                  )

          fills = bodyRestructured # Fills.fromBody
        in
          bodyRestructured
            # Holes.fill fills
      Maybe.Nothing -> Tree.singleton Xml.Root
    Either.Left _ -> Tree.singleton Xml.Root
  Either.Left _ -> Tree.singleton Xml.Root

fromDocumentName :: String -> String -> Targets.Targets -> Effect.Effect (Tree.Tree Xml.Element)
fromDocumentName name sourceDirectory targets =
  bind (Xml.fromDocumentName name sourceDirectory)
    ( \content ->
        pure $ fromDocumentContent' content targets sourceDirectory false
    )
