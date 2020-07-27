module Bone.Descriptor.Element.Targets (fromFleshItems, merge, empty, Targets, TargetProperties(..), TargetType(..)) where

import Xml as Xml
import Data.Map as Map
import Data.Array as Array
import Data.Maybe as Maybe
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String as String
import Data.Either as Either
import Prelude
import Data.Tuple as Tuple

data TargetType
  = Append
  | Prepend
  | Set

data TargetProperties
  = TargetProperties TargetType (Maybe.Maybe Xml.Attributes) (Maybe.Maybe String)

type Targets
  = Map.Map String TargetProperties

fromFleshItems :: Array Xml.Element -> Targets
fromFleshItems items = fromFleshItems' items Map.empty

fromFleshItems' :: Array Xml.Element -> Targets -> Targets
fromFleshItems' items targets = case items # Array.head of
  Maybe.Just (Xml.Flesh currentItem) ->
    let
      restOfItems = Array.tail items # Maybe.fromMaybe []

      newTargets =
        let
          boneDescriptorElementIds = case Regex.regex "\\s" RegexFlags.global of
            Either.Right regex -> Regex.split regex currentItem.for # Array.filter (not String.null)
            Either.Left _ -> []
        in
          Array.foldl
            ( \acc item ->
                let
                  Tuple.Tuple id targetType = case String.take 3 item of
                    "(=)" -> Tuple.Tuple (String.drop 3 item) Set
                    "(<)" -> Tuple.Tuple (String.drop 3 item) Prepend
                    "(>)" -> Tuple.Tuple (String.drop 3 item) Append
                    _ -> Tuple.Tuple item Append

                  newTargetProperties = TargetProperties targetType currentItem.attributes currentItem.content
                in
                  merge (Map.singleton id newTargetProperties) acc
            )
            targets
            boneDescriptorElementIds
    in
      fromFleshItems' restOfItems newTargets
  _ -> targets

merge :: Targets -> Targets -> Targets
merge =
  Map.unionWith
    ( \(TargetProperties type_ attributes content) (TargetProperties oldType oldAttributes oldContent) -> case oldType of
        Set -> TargetProperties type_ (Maybe.maybe oldAttributes (\justAttributes -> Maybe.Just justAttributes) attributes) (Maybe.maybe oldContent (\justContent -> Maybe.Just justContent) content)
        Prepend ->
          TargetProperties oldType
            (attributes # Maybe.maybe oldAttributes (\justAttributes -> oldAttributes # Maybe.maybe attributes (\justOldAttributes -> Maybe.Just $ Xml.mergeAttributes justAttributes justOldAttributes)))
            (content # Maybe.maybe oldContent (\justContent -> oldContent # Maybe.maybe content (\justOldContent -> Maybe.Just $ justContent <> justOldContent)))
        Append ->
          TargetProperties oldType
            (attributes # Maybe.maybe oldAttributes (\justAttributes -> oldAttributes # Maybe.maybe attributes (\justOldAttributes -> Maybe.Just $ Xml.mergeAttributes justOldAttributes justAttributes)))
            (content # Maybe.maybe oldContent (\justContent -> oldContent # Maybe.maybe content (\justOldContent -> Maybe.Just $ justOldContent <> justContent)))
    )

empty :: Targets
empty = Map.empty
