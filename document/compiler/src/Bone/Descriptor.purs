module Bone.Descriptor where

import Prelude
import Data.String as String
import Data.Array as Array

type Element
  = { name :: String
    , id :: String
    , attributes :: String
    , xClass :: String
    , xId :: String
    , hasClosingTag :: Boolean
    }

data ElementProperty
  = Name
  | Id
  | Attributes
  | XClass
  | XId
  | HasClosingTag

blankElement :: Element
blankElement =
  { name: ""
  , id: ""
  , attributes: ""
  , xClass: ""
  , xId: ""
  , hasClosingTag: true
  }

type Model
  = { mode :: Mode
    , currentCharacter :: String
    , restOfCharacters :: String
    , currentElement :: Element
    , elements :: Array Element
    }

data CharacterType
  = StartOfAttributes
  | EndOfAttributes
  | StartOfXClass
  | StartOfXId
  | StartOfId
  | Whitespace
  | Null
  | Other

instance eqCharacterType :: Eq CharacterType where
  eq Whitespace Whitespace = true
  eq Null Null = true
  eq Other Other = true
  eq StartOfId StartOfId = true
  eq StartOfXId StartOfXId = true
  eq StartOfXClass StartOfXClass = true
  eq EndOfAttributes EndOfAttributes = true
  eq StartOfAttributes StartOfAttributes = true
  eq _ _ = false

data Mode
  = Append ElementProperty

getCharacterType :: String -> CharacterType
getCharacterType character = case character of
  "[" -> StartOfAttributes
  "]" -> EndOfAttributes
  "." -> StartOfXClass
  "#" -> StartOfXId
  "@" -> StartOfId
  "" -> Null
  _ ->
    if String.trim character # String.null then
      Whitespace
    else
      Other

toElements :: String -> Array Element
toElements descriptor =
  toElements'
    { mode: Append Name
    , currentCharacter: String.take 1 descriptor
    , restOfCharacters: String.drop 1 descriptor
    , currentElement: blankElement
    , elements: []
    }

toElements' :: Model -> Array Element
toElements' model =
  if String.null model.currentCharacter then
    (endElement model).elements
  else
    let
      characterType = getCharacterType model.currentCharacter

      newModel = case model.mode of
        Append Name ->
          if characterType == Other then
            appendElementProperty Name model
          else
            handleStartOfElement characterType model
        Append Attributes ->
          if characterType == EndOfAttributes then
            model { mode = Append Name }
          else
            appendElementProperty Attributes model
        Append XClass ->
          if characterType == Other then
            appendElementProperty XClass model
          else
            handleStartOfElement characterType model { currentElement = model.currentElement { xClass = model.currentElement.xClass <> " " } }
        Append XId ->
          if characterType == Other then
            appendElementProperty XId model
          else
            handleStartOfElement characterType model
        Append Id ->
          if characterType == Other then
            appendElementProperty Id model
          else
            handleStartOfElement characterType model
        _ -> model
    in
      toElements'
        ( newModel
            { currentCharacter = model.restOfCharacters # String.take 1
            , restOfCharacters = model.restOfCharacters # String.drop 1
            }
        )

handleStartOfElement :: CharacterType -> Model -> Model
handleStartOfElement characterType model = case characterType of
  StartOfAttributes -> model { mode = Append Attributes }
  StartOfXClass ->
    let
      nextCharacterType = getCharacterType $ model.restOfCharacters # String.take 1
    in
      if nextCharacterType == Whitespace || nextCharacterType == Null then
        model { currentElement = model.currentElement { hasClosingTag = false }, mode = Append Name }
      else
        model { mode = Append XClass }
  StartOfXId -> model { mode = Append XId }
  StartOfId -> model { mode = Append Id }
  Whitespace -> endElement model
  _ -> model

endElement :: Model -> Model
endElement model = model { elements = Array.snoc model.elements model.currentElement, mode = Append Name, currentElement = blankElement }

appendElementProperty :: ElementProperty -> Model -> Model
appendElementProperty property model =
  let
    newElement = case property of
      Name -> model.currentElement { name = model.currentElement.name <> model.currentCharacter }
      Id -> model.currentElement { id = model.currentElement.id <> model.currentCharacter }
      XId -> model.currentElement { xId = model.currentElement.xId <> model.currentCharacter }
      XClass -> model.currentElement { xClass = model.currentElement.xClass <> model.currentCharacter }
      Attributes -> model.currentElement { attributes = model.currentElement.attributes <> model.currentCharacter }
      _ -> model.currentElement
  in
    model { currentElement = newElement }
