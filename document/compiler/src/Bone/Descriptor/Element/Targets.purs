module Bone.Descriptor.Element.Targets where

import Xml as Xml
import Data.Map as Map
import Data.Array as Array
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Pattern as StringPattern
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

          boneDescriptorElementIds = String.split (StringPattern.Pattern " ") currentItem.targets
        in
          Array.foldl (\acc id -> acc # Map.insert id content) targets boneDescriptorElementIds
    in
      fromFleshItems' restOfItems newTargets
  _ -> targets

merge :: Targets -> Targets -> Targets
merge = Map.union
