module State exposing (initialize, subscriptions, update)

import Browser.Events
import Core exposing (FlagType, KeyInteractionType(..), Model, Msg(..))
import Document
import Document.Element exposing (Element(..))
import Json.Decode
import Rest


initialize : FlagType -> ( Model, Cmd Msg )
initialize flags =
    ( { document = Document.fromString flags.content
      , pageName = flags.pageName
      , mode = Core.Default
      , hotkeysEnabled = True
      , elementEditingEnabled = True
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
                                    Document.replaceElement index
                                        (\element ->
                                            case element of
                                                Document.Element.Bone bone ->
                                                    Document.Element.Bone { bone | descriptor = descriptor }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }

                        SetFleshTargets index targets ->
                            let
                                newDocument =
                                    Document.replaceElement index
                                        (\element ->
                                            case element of
                                                Document.Element.Flesh flesh ->
                                                    Document.Element.Flesh { flesh | targets = targets }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }

                        SetFleshContent index content ->
                            let
                                newDocument =
                                    Document.replaceElement index
                                        (\element ->
                                            case element of
                                                Document.Element.Flesh flesh ->
                                                    Document.Element.Flesh { flesh | content = content }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }

                        SetMode mode ->
                            { model | mode = mode }

                        SelectElement id ->
                            let
                                newDocument =
                                    Document.mapElements
                                        (\element ->
                                            case element of
                                                Document.Element.Bone bone ->
                                                    if bone.id == id then
                                                        Document.Element.Bone { bone | selected = True }

                                                    else
                                                        Document.Element.Bone { bone | selected = False }

                                                Document.Element.Flesh flesh ->
                                                    if flesh.id == id then
                                                        Document.Element.Flesh { flesh | selected = True }

                                                    else
                                                        Document.Element.Flesh { flesh | selected = False }

                                                _ ->
                                                    element
                                        )
                                        document
                            in
                            { model | document = Just newDocument }

                        KeyInteraction _ key _ ->
                            if model.hotkeysEnabled then
                                case model.mode of
                                    Core.Default ->
                                        model

                                    _ ->
                                        case key of
                                            "Escape" ->
                                                { model | mode = Core.Default }

                                            _ ->
                                                model

                            else
                                model

                        ToggleHotkeysEnabled ->
                            { model | hotkeysEnabled = not model.hotkeysEnabled }

                        ElementClick id ->
                            case model.mode of
                                Core.Selection type_ purpose ->
                                    case purpose of
                                        Core.Removal ->
                                            let
                                                newDocument =
                                                    Document.removeElement id document
                                            in
                                            { model
                                                | document = Just newDocument
                                                , mode = Core.Default
                                            }

                                        Core.Addition additionType ->
                                            case type_ of
                                                Core.Bone ->
                                                    case additionType of
                                                        Core.Before ->
                                                            let
                                                                newDocument =
                                                                    Document.addElementBeforeElement id Document.Element.emptyBone document
                                                            in
                                                            { model
                                                                | document = Just newDocument
                                                                , mode = Core.Default
                                                            }

                                                        Core.After ->
                                                            let
                                                                newDocument =
                                                                    Document.addElementAfterElement id Document.Element.emptyBone document
                                                            in
                                                            { model
                                                                | document = Just newDocument
                                                                , mode = Core.Default
                                                            }

                                                        Core.InsideFirst ->
                                                            let
                                                                newDocument =
                                                                    Document.addElementInsideElementAsFirstChild id Document.Element.emptyBone document
                                                            in
                                                            { model
                                                                | document = Just newDocument
                                                                , mode = Core.Default
                                                            }

                                                        Core.InsideLast ->
                                                            let
                                                                newDocument =
                                                                    Document.addElementInsideElementAsLastChild id Document.Element.emptyBone document
                                                            in
                                                            { model
                                                                | document = Just newDocument
                                                                , mode = Core.Default
                                                            }

                                                Core.Flesh ->
                                                    case additionType of
                                                        Core.Before ->
                                                            let
                                                                newDocument =
                                                                    Document.addElementBeforeElement id Document.Element.emptyFlesh document
                                                            in
                                                            { model
                                                                | document = Just newDocument
                                                                , mode = Core.Default
                                                            }

                                                        Core.After ->
                                                            let
                                                                newDocument =
                                                                    Document.addElementAfterElement id Document.Element.emptyFlesh document
                                                            in
                                                            { model
                                                                | document = Just newDocument
                                                                , mode = Core.Default
                                                            }

                                                        _ ->
                                                            model

                                                _ ->
                                                    model

                                _ ->
                                    model
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
