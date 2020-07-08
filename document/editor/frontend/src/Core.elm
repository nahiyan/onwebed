module Core exposing (FlagType, KeyInteractionType(..), Mode(..), Model, Msg(..), SelectionType(..))

import Document.Element exposing (Element)
import Tree exposing (Tree)


type SelectionType
    = Before
    | After
    | InsideFirst
    | InsideLast


type Mode
    = Default
    | BoneSelectionForAddition SelectionType
    | ElementSelectionForRemoval


type alias Model =
    { document : Maybe (Tree Element)
    , pageName : String
    , mode : Mode
    , elementSelection : Int
    , hotkeysEnabled : Bool
    }


type KeyInteractionType
    = Up
    | Down
    | Press


type Msg
    = SetBoneDescriptor Int String
    | SetFleshTargets Int String
    | SetFleshContent Int String
    | MenuItemClick String
    | SetMode Mode
    | SelectElement Int
    | RemoveElement Int
    | KeyInteraction KeyInteractionType String Bool
    | ToggleHotkeysEnabled


type alias FlagType =
    { pageName : String
    , content : String
    }
