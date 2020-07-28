module Bone.Descriptor.Element.Targets (fromFleshItems, merge, empty, Targets, TargetProperties(..), TargetType(..), apply) where

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
  = Map.Map String (Array TargetProperties)

instance showTargetProperties :: Show TargetProperties where
  show (TargetProperties a b c) = show a <> " " <> show b <> " " <> show c

instance showTargetType :: Show TargetType where
  show Append = "Append"
  show Prepend = "Prepend"
  show Set = "Set"

instance eqTargetProperties :: Eq TargetProperties where
  eq a b = show a == show b

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
                  merge (Map.singleton id [ newTargetProperties ]) acc
            )
            targets
            boneDescriptorElementIds
    in
      fromFleshItems' restOfItems newTargets
  _ -> targets

merge :: Targets -> Targets -> Targets
merge =
  Map.unionWith
    ( \new old ->
        old <> new
    )

apply :: Xml.Attributes -> String -> Array TargetProperties -> Tuple.Tuple Xml.Attributes String
apply attributes content targetProperties =
  targetProperties
    # Array.foldl
        ( \(Tuple.Tuple attributes_ content_) (TargetProperties type_ attributes__ content__) ->
            let
              newAttributes =
                attributes__
                  # Maybe.maybe attributes_
                      ( \justAttributes -> case type_ of
                          Prepend -> Xml.mergeAttributes justAttributes attributes_
                          Append -> Xml.mergeAttributes attributes_ justAttributes
                          Set -> justAttributes
                      )

              newContent =
                content__
                  # Maybe.maybe content_
                      ( \justContent -> case type_ of
                          Prepend -> justContent <> content_
                          Append -> content_ <> justContent
                          Set -> justContent
                      )
            in
              Tuple.Tuple newAttributes newContent
        )
        (Tuple.Tuple attributes content)

empty :: Targets
empty = Map.empty
