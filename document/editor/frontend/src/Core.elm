module Core exposing (FlagType, Model)

import Document.Element exposing (Element)
import Tree exposing (Tree)


type alias Model =
    { document : Maybe (Tree Element)
    , pageName : String
    }


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
