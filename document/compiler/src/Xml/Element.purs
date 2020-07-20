module Xml.Element (Element(..), Attribute) where

import Data.Tuple (Tuple)
-- import Tree as Tree
import Data.Maybe as Maybe
import Prelude ((#), bind, (==), map, (<>), ($), pure)
import Data.Array as Array
import Data.Argonaut.Parser as Parser

type Attribute
  = Tuple String String

data Element
  = Element { name :: String }
  | Bone { descriptor :: String }
  | Flesh { targets :: String, content :: String }
  | Text String
  | Document
  | Head
  | Body
  | Root
