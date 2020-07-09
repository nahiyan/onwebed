module Core exposing (AdditionType(..), FlagType, KeyInteractionType(..), Mode(..), Model, Msg(..), SelectionPurpose(..), SelectionType(..))

import Document.Element exposing (Element)
import Tree exposing (Tree)


type AdditionType
    = Before
    | After
    | InsideFirst
    | InsideLast


type SelectionPurpose
    = Removal
    | Addition AdditionType


type SelectionType
    = Bone
    | Flesh
    | BoneAndFlesh


type Mode
    = Default
    | Selection SelectionType SelectionPurpose


type alias Model =
    { document : Maybe (Tree Element)
    , pageName : String
    , mode : Mode
    , hotkeysEnabled : Bool
    , elementEditingEnabled : Bool
    }


type KeyInteractionType
    = Up
    | Down
    | Press


type Msg
    = SetBoneDescriptor Int String
    | SetFleshTargets Int String
    | SetFleshContent Int String
    | SetMode Mode
    | SelectElement Int
    | KeyInteraction KeyInteractionType String Bool
    | ToggleHotkeysEnabled
    | ElementClick Int


type alias FlagType =
    { pageName : String
    , content : String
    }
