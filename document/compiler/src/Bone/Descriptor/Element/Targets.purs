module Bone.Descriptor.Element.Targets (fromFleshItems, merge, empty, Targets) where

import Xml as Xml
import Data.Map as Map
import Data.Array as Array
import Data.Maybe as Maybe
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String as String
import Data.Either as Either
import Prelude

type Targets
  = Map.Map String String

fromFleshItems :: Array Xml.Element -> Targets
fromFleshItems items = fromFleshItems' items Map.empty

fromFleshItems' :: Array Xml.Element -> Targets -> Targets
fromFleshItems' items targets = case items # Array.head of
  Maybe.Just (Xml.Flesh currentItem) ->
    let
      restOfItems = Array.tail items # Maybe.fromMaybe []

      newTargets =
        let
          content = currentItem.content

          boneDescriptorElementIds = case Regex.regex "\\s" RegexFlags.global of
            Either.Right regex -> Regex.split regex currentItem.targets # Array.filter (not String.null)
            Either.Left _ -> []
        in
          Array.foldl
            ( \acc id ->
                Map.alter
                  ( Maybe.maybe (Maybe.Just content)
                      (\oldValue -> Maybe.Just $ oldValue <> content)
                  )
                  id
                  acc
            )
            targets
            boneDescriptorElementIds
    in
      fromFleshItems' restOfItems newTargets
  _ -> targets

merge :: Targets -> Targets -> Targets
merge = Map.unionWith (\newValue oldValue -> oldValue <> newValue)

empty :: Targets
empty = Map.empty
