module Core exposing (FlagType, Model, Msg(..))

import Document.Element exposing (Element)
import Tree exposing (Tree)


type alias Model =
    { document : Maybe (Tree Element)
    , pageName : String
    }


type Msg
    = SetBoneDescriptor Int String
    | SetFleshTargets Int String
    | SetFleshContent Int String


type alias FlagType =
    { pageName : String
    , content : String
    }



-- { document : List DocumentElement
-- , menu : List MenuItem
-- , status : DocumentStatus
-- , menuMessage : Maybe String
-- , pageName : String
-- , keyInteraction : Bool
-- }
