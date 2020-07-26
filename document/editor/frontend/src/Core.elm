module Core exposing (FlagType, KeyInteractionType(..), Mode(..), Model, Msg(..), SaveState(..))

import Document exposing (Document)
import Document.Element
import Http


type Mode
    = Default
    | Selection Document.Element.SelectionType Document.Element.SelectionPurpose
    | MarkupEditing


type SaveState
    = Saving
    | NoSaveRequired
    | SaveRequired


type alias Model =
    { document : Document
    , machineName : String
    , mode : Mode
    , hotkeysEnabled : Bool
    , elementEditingEnabled : Bool
    , markup : String
    , filter : Document.Element.SelectionType
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
    | SetDocumentName String
    | SetMode Mode
    | SelectElement Int
    | KeyInteraction KeyInteractionType String Bool
    | ToggleHotkeysEnabled
    | ElementClick Int
    | PrepareMarkupEditing
    | StartMarkupEditing String
    | EndMarkupEditing
    | UpdateMarkup String
    | SetFilter Document.Element.SelectionType
    | ApplyMarkup
    | RebuildDocument String
    | ExpireBabyElement Int
    | SetNextBabyId Int
    | SaveDocument
    | SaveDocumentResult (Result Http.Error String)
    | AddElement Document.Element.SelectionType Document.Element.AdditionType


type alias FlagType =
    { fileName : String
    , content : String
    }
