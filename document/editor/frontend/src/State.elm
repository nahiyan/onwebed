module State exposing (initialize, subscriptions, update)

import Browser.Events
import Core exposing (FlagType, KeyInteractionType(..), Mode(..), Model, Msg(..))
import Document.Element exposing (Element(..))
import Document.Elements.Tree as Tree
import Json.Decode
import Rest


initialize : FlagType -> ( Model, Cmd Msg )
initialize flags =
    ( { document = Tree.fromString flags.content
      , pageName = flags.pageName
      , mode = Default
      , elementSelection = 0
      , hotkeysEnabled = True
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

                        MenuItemClick menuItemMachineName ->
                            case menuItemMachineName of
                                "add_element" ->
                                    model

                                _ ->
                                    model

                        SetMode mode ->
                            { model | mode = mode }

                        SelectElement id ->
                            let
                                newDocument =
                                    Tree.mapElements
                                        (\element ->
                                            case element of
                                                Bone bone ->
                                                    if bone.id == id then
                                                        Bone { bone | selected = True }

                                                    else
                                                        Bone { bone | selected = False }

                                                Flesh flesh ->
                                                    if flesh.id == id then
                                                        Flesh { flesh | selected = True }

                                                    else
                                                        Flesh { flesh | selected = False }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }

                        RemoveElement id ->
                            let
                                newDocument =
                                    Tree.removeElement id document
                            in
                            { model
                                | document = Just newDocument
                                , mode = Default
                            }

                        KeyInteraction _ key _ ->
                            if model.hotkeysEnabled then
                                case model.mode of
                                    Default ->
                                        model

                                    _ ->
                                        case key of
                                            "Escape" ->
                                                { model | mode = Default }

                                            _ ->
                                                model

                            else
                                model

                        ToggleHotkeysEnabled ->
                            { model | hotkeysEnabled = not model.hotkeysEnabled }
    in
    ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Json.Decode.map2
                (KeyInteraction Down)
                Rest.keyDecoder
                Rest.shiftKeyDecoder
            )
        ]
