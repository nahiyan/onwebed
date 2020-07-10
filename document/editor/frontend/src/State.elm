module State exposing (initialize, subscriptions, update)

import Browser.Events
import Core exposing (FlagType, KeyInteractionType(..), Model, Msg(..))
import Document
import Document.Body
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
        document =
            model.document

        newModel =
            case document.body of
                Nothing ->
                    model

                Just body ->
                    case message of
                        SetBoneDescriptor index descriptor ->
                            let
                                newBody =
                                    Document.Body.replaceElement index
                                        (\element ->
                                            case element of
                                                Document.Element.Bone bone ->
                                                    Document.Element.Bone { bone | descriptor = descriptor }

                                                _ ->
                                                    element
                                        )
                                        body
                            in
                            { model | document = { document | body = Just newBody } }

                        SetFleshTargets index targets ->
                            let
                                newBody =
                                    Document.Body.replaceElement index
                                        (\element ->
                                            case element of
                                                Document.Element.Flesh flesh ->
                                                    Document.Element.Flesh { flesh | targets = targets }

                                                _ ->
                                                    element
                                        )
                                        body
                            in
                            { model | document = { document | body = Just newBody } }

                        SetFleshContent index content ->
                            let
                                newBody =
                                    Document.Body.replaceElement index
                                        (\element ->
                                            case element of
                                                Document.Element.Flesh flesh ->
                                                    Document.Element.Flesh { flesh | content = content }

                                                _ ->
                                                    element
                                        )
                                        body
                            in
                            { model | document = { document | body = Just newBody } }

                        SetMode mode ->
                            { model | mode = mode }

                        SelectElement id ->
                            let
                                newBody =
                                    Document.Body.mapElements
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
                                        body
                            in
                            { model | document = { document | body = Just newBody } }

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
                                                newBody =
                                                    Document.Body.removeElement id body
                                            in
                                            { model
                                                | document = { document | body = Just newBody }
                                                , mode = Core.Default
                                            }

                                        Core.Addition additionType ->
                                            case type_ of
                                                Core.Bone ->
                                                    case additionType of
                                                        Core.Before ->
                                                            let
                                                                newBody =
                                                                    Document.Body.addElementBeforeElement id Document.Element.emptyBone body
                                                            in
                                                            { model
                                                                | document = { document | body = Just newBody }
                                                                , mode = Core.Default
                                                            }

                                                        Core.After ->
                                                            let
                                                                newBody =
                                                                    Document.Body.addElementAfterElement id Document.Element.emptyBone body
                                                            in
                                                            { model
                                                                | document = { document | body = Just newBody }
                                                                , mode = Core.Default
                                                            }

                                                        Core.InsideFirst ->
                                                            let
                                                                newBody =
                                                                    Document.Body.addElementInsideElementAsFirstChild id Document.Element.emptyBone body
                                                            in
                                                            { model
                                                                | document = { document | body = Just newBody }
                                                                , mode = Core.Default
                                                            }

                                                        Core.InsideLast ->
                                                            let
                                                                newBody =
                                                                    Document.Body.addElementInsideElementAsLastChild id Document.Element.emptyBone body
                                                            in
                                                            { model
                                                                | document = { document | body = Just newBody }
                                                                , mode = Core.Default
                                                            }

                                                Core.Flesh ->
                                                    case additionType of
                                                        Core.Before ->
                                                            let
                                                                newBody =
                                                                    Document.Body.addElementBeforeElement id Document.Element.emptyFlesh body
                                                            in
                                                            { model
                                                                | document = { document | body = Just newBody }
                                                                , mode = Core.Default
                                                            }

                                                        Core.After ->
                                                            let
                                                                newBody =
                                                                    Document.Body.addElementAfterElement id Document.Element.emptyFlesh body
                                                            in
                                                            { model
                                                                | document = { document | body = Just newBody }
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
