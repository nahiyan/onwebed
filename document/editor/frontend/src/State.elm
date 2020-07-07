module State exposing (initialize, subscriptions, update)

import Core exposing (FlagType, Model)
import Document.Elements.Tree as Tree


initialize : FlagType -> ( Model, Cmd msg )
initialize flags =
    ( { document = Tree.fromString flags.content
      , pageName = flags.pageName
      }
    , Cmd.none
    )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
