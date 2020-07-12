module Core exposing (AdditionType(..), FlagType, KeyInteractionType(..), Mode(..), Model, Msg(..), SaveState(..), SelectionPurpose(..), SelectionType(..))

import Document exposing (Document)
import Http


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
    | All


type Mode
    = Default
    | Selection SelectionType SelectionPurpose
    | MarkupEditing


type SaveState
    = Saving
    | NoSaveRequired
    | SaveRequired


type alias Model =
    { document : Document
    , fileName : String
    , mode : Mode
    , hotkeysEnabled : Bool
    , elementEditingEnabled : Bool
    , markup : String
    , filter : SelectionType
    , nextBabyId : Maybe Int
    , saveState : SaveState
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
    | PrepareMarkupEditing
    | StartMarkupEditing String
    | EndMarkupEditing
    | UpdateMarkup String
    | SetFilter SelectionType
    | ApplyMarkup
    | RebuildDocument String
    | ExpireBabyElement Int
    | SetNextBabyId Int
    | SaveDocument
    | SaveDocumentResult (Result Http.Error String)


type alias FlagType =
    { fileName : String
    , content : String
    }
