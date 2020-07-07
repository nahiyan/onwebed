module State exposing (initialize, subscriptions, update)

import Core exposing (FlagType, Model, Msg(..))
import Document.Element exposing (Element(..))
import Document.Elements.Tree as Tree


initialize : FlagType -> ( Model, Cmd Msg )
initialize flags =
    ( { document = Tree.fromString flags.content
      , pageName = flags.pageName
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        newModel =
            case model.document of
                Nothing ->
                    model

                Just document ->
                    case message of
                        SetBoneDescriptor index descriptor ->
                            let
                                newDocument =
                                    Tree.replaceElement index
                                        (\element ->
                                            case element of
                                                Bone bone ->
                                                    Bone { bone | descriptor = descriptor }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }

                        SetFleshTargets index targets ->
                            let
                                newDocument =
                                    Tree.replaceElement index
                                        (\element ->
                                            case element of
                                                Flesh flesh ->
                                                    Flesh { flesh | targets = targets }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }

                        SetFleshContent index content ->
                            let
                                newDocument =
                                    Tree.replaceElement index
                                        (\element ->
                                            case element of
                                                Flesh flesh ->
                                                    Flesh { flesh | content = content }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }
    in
    ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
