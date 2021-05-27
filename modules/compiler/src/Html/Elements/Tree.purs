module Html.Elements.Tree (fromBoneDescriptorElements, fromDocumentContent, fromDocumentName) where

import Prelude
import Bone.Descriptor as Descriptor
import Bone.Descriptor.Element.Targets as Targets
import Data.Argonaut.Decode as JsonDecode
import Data.Argonaut.Parser as Parser
import Data.Array as Array
import Data.Either as Either
import Data.Map as Map
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple as Tuple
import Document.Body.Fills as Fills
import Document.Body.Holes as Holes
import Effect as Effect
import Effect.Unsafe as EffectUnsafe
import Foreign.Object as FObject
import Tree as Tree
import Tree.Zipper as Zipper
import Xml as Xml

lastHtmlElementZipper :: Zipper.Zipper Xml.Element -> Zipper.Zipper Xml.Element
lastHtmlElementZipper zipper = lastHtmlElementZipper' (zipper # Zipper.lastDescendant)

lastHtmlElementZipper' :: Zipper.Zipper Xml.Element -> Zipper.Zipper Xml.Element
lastHtmlElementZipper' zipper = case zipper # Zipper.tree of
  Tree.Tree (Xml.Element _) children -> zipper
  _ -> case (zipper # Zipper.backward) of
    Maybe.Just newZipper -> lastHtmlElementZipper' newZipper
    Maybe.Nothing -> zipper # Zipper.root

fromBoneDescriptorElements :: Array Descriptor.Element -> Targets.Targets -> Array (Tree.Tree Xml.Element) -> String -> Tree.Tree Xml.Element
fromBoneDescriptorElements elements targets boneChildren sourceDirectory =
  Array.foldl
    ( \acc element ->
        let
          target = targets # Map.lookup element.id

          attributes_ =
            Xml.attributesFromString element.attributes
              # (if element.htmlId # String.null then identity else FObject.insert "id" element.htmlId)
              # (if element.htmlClass # String.null then identity else FObject.insert "class" element.htmlClass)

          Tuple.Tuple attributes content = target # Maybe.maybe (Tuple.Tuple attributes_ "") (\targetProperties -> targetProperties # Targets.apply attributes_ "")

          newTreeChildren =
            if content # String.null then
              if element.hasClosingTag && element.name /= "fill" && element.name /= "hole" then
                [ Tree.singleton (Xml.Text "") ]
              else
                []
            else
              [ Tree.singleton (Xml.Text content) ]
        in
          acc # Zipper.fromTree
            # lastHtmlElementZipper
            # Zipper.mapTree
                ( \(Tree.Tree label children) ->
                    let
                      newChildren =
                        if element.name /= "page" then
                          Array.snoc children $ Tree.tree (Xml.Element { name: element.name, attributes: attributes }) newTreeChildren
                        else case attributes # FObject.lookup "id" of
                          Maybe.Just id -> children <> (Tree.children $ EffectUnsafe.unsafePerformEffect $ fromDocumentName id sourceDirectory targets)
                          Maybe.Nothing -> Array.snoc children $ Tree.singleton Xml.Blank
                    in
                      Tree.tree label newChildren
                )
            # Zipper.toTree
    )
    (Tree.singleton Xml.Root)
    elements
    # Zipper.fromTree
    # lastHtmlElementZipper
    # Zipper.mapTree
        ( \(Tree.Tree label children) ->
            Tree.tree label (children <> boneChildren)
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

          processChildren children =
            children
              # Array.foldl
                  ( \acc child -> case child # Tree.label of
                      Xml.Bone bone -> acc <> (fromBoneDescriptorElements (Descriptor.toElements bone.descriptor) combinedTargets (child # Tree.children) sourceDirectory # Tree.children)
                      _ -> child # Array.snoc acc
                  )
                  []

          bodyRestructured =
            body
              # Tree.restructure identity
                  ( \label children -> case label of
                      Xml.Body -> Tree.tree Xml.Root $ processChildren children
                      Xml.Bone bone -> Tree.tree label $ processChildren children
                      _ -> Tree.singleton Xml.Blank
                  )
        in
          bodyRestructured
            # if shouldFillHoles then
                Holes.fill $ bodyRestructured # Fills.fromBody
              else
                identity
      Maybe.Nothing -> Tree.singleton Xml.Root
    Either.Left _ -> Tree.singleton Xml.Root
  Either.Left _ -> Tree.singleton Xml.Root

fromDocumentName :: String -> String -> Targets.Targets -> Effect.Effect (Tree.Tree Xml.Element)
fromDocumentName name sourceDirectory targets = do
  content <- Xml.fromDocumentName name sourceDirectory
  pure $ fromDocumentContent' content targets sourceDirectory false
