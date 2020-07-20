module Bone.Descriptor.Element.Targets where

import Xml as Xml
import Data.Map as Map
import Data.Array as Array
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Pattern as StringPattern
import Prelude

fromFleshItems :: Array Xml.Element -> Map.Map String String
fromFleshItems items = fromFleshItems' items Map.empty

fromFleshItems' :: Array Xml.Element -> Map.Map String String -> Map.Map String String
fromFleshItems' items targets = case items # Array.head of
  Maybe.Just (Xml.Flesh currentItem) ->
    let
      restOfItems = Array.tail items # Maybe.fromMaybe []

      newTargets =
        let
          content = currentItem.content

          boneDescriptorElementIds = String.split (StringPattern.Pattern " ") currentItem.targets
        in
          Array.foldl (\acc id -> acc # Map.insert id content) targets boneDescriptorElementIds
    in
      fromFleshItems' restOfItems newTargets
  _ -> targets
