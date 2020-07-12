port module State exposing (initialize, subscriptions, update)

import Browser.Events
import Core exposing (FlagType, KeyInteractionType(..), Model, Msg(..))
import Delay
import Document
import Document.Body
import Document.Element exposing (Element(..))
import Http
import Json.Decode
import Random
import Rest
import Tree



-- Overlay for modals


port overlay : Bool -> Cmd msg



-- Markup editing


port setupMarkupEditor : String -> Cmd msg


port documentToMarkup : String -> Cmd msg


port markupToDocument : String -> Cmd msg


port documentToMarkupResult : (String -> msg) -> Sub msg


port markupToDocumentResult : (String -> msg) -> Sub msg


port updateMarkup : (String -> msg) -> Sub msg


generateNextBabyId : Cmd Msg
generateNextBabyId =
    Random.generate SetNextBabyId (Random.int Random.minInt Random.maxInt)


initialize : FlagType -> ( Model, Cmd Msg )
initialize flags =
    ( { document = Document.fromString flags.content
      , fileName = flags.fileName
      , mode = Core.Default
      , hotkeysEnabled = True
      , elementEditingEnabled = True
      , markup = ""
      , filter = Core.All
      , nextBabyId = Nothing
      , saveState = Core.NoSaveRequired
      }
    , generateNextBabyId
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        document =
            model.document

        body =
            case document.body of
                Nothing ->
                    Tree.singleton Document.Element.Root

                Just justBody ->
                    justBody

        applyInsertion =
            \newBody ->
                ( { model
                    | document = { document | body = Just newBody }
                    , mode = Core.Default
                    , saveState = Core.SaveRequired
                  }
                , Cmd.batch
                    [ Delay.after
                        2500
                        Delay.Millisecond
                        (ExpireBabyElement (Maybe.withDefault 0 model.nextBabyId))
                    , generateNextBabyId
                    ]
                )

        ( newModel, command ) =
            case message of
                SaveDocument ->
                    let
                        url =
                            "/save/" ++ model.fileName

                        body_ =
                            Http.stringBody "application/json" (Document.toJson model.document)

                        expect =
                            Http.expectString SaveDocumentResult
                    in
                    ( model, Http.post { url = url, body = body_, expect = expect } )

                SaveDocumentResult result ->
                    case result of
                        Ok "success" ->
                            ( { model | saveState = Core.NoSaveRequired }, Cmd.none )

                        _ ->
                            ( { model | saveState = Core.SaveRequired }, Cmd.none )

                SetNextBabyId id ->
                    ( { model | nextBabyId = Just id }, Cmd.none )

                ExpireBabyElement id ->
                    let
                        newBody =
                            Document.Body.expireBabyElement id body
                    in
                    ( { model | document = { document | body = Just newBody } }, Cmd.none )

                ApplyMarkup ->
                    ( model, markupToDocument model.markup )

                RebuildDocument json ->
                    ( { model | document = Document.fromString json, mode = Core.Default }, overlay False )

                SetFilter selectionType ->
                    ( { model | filter = selectionType }, Cmd.none )

                PrepareMarkupEditing ->
                    ( model
                    , Cmd.batch
                        [ documentToMarkup (Document.toJson model.document)
                        , overlay True
                        ]
                    )

                StartMarkupEditing markup ->
                    ( { model | markup = markup, mode = Core.MarkupEditing }, setupMarkupEditor markup )

                UpdateMarkup markup ->
                    ( { model | markup = markup, saveState = Core.SaveRequired }, Cmd.none )

                EndMarkupEditing ->
                    ( { model | mode = Core.Default }, overlay False )

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
                    ( { model | document = { document | body = Just newBody }, saveState = Core.SaveRequired }, Cmd.none )

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
                    ( { model | document = { document | body = Just newBody }, saveState = Core.SaveRequired }, Cmd.none )

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
                    ( { model | document = { document | body = Just newBody }, saveState = Core.SaveRequired }, Cmd.none )

                SetMode mode ->
                    ( { model | mode = mode }, Cmd.none )

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
                    ( { model | document = { document | body = Just newBody } }, Cmd.none )

                KeyInteraction _ key _ ->
                    if model.hotkeysEnabled then
                        case model.mode of
                            Core.Default ->
                                ( model, Cmd.none )

                            _ ->
                                case key of
                                    "Escape" ->
                                        ( { model | mode = Core.Default }, overlay False )

                                    _ ->
                                        ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

                ToggleHotkeysEnabled ->
                    ( { model | hotkeysEnabled = not model.hotkeysEnabled }, Cmd.none )

                ElementClick id ->
                    case model.mode of
                        Core.Selection type_ purpose ->
                            case purpose of
                                Core.Removal ->
                                    let
                                        newBody =
                                            Document.Body.removeElement id body
                                    in
                                    ( { model
                                        | document = { document | body = Just newBody }
                                        , mode = Core.Default
                                        , saveState = Core.SaveRequired
                                      }
                                    , Cmd.none
                                    )

                                Core.Addition additionType ->
                                    case type_ of
                                        Core.Bone ->
                                            case additionType of
                                                Core.Before ->
                                                    let
                                                        newBody =
                                                            Document.Body.addElementBeforeElement id (Document.Element.emptyBone model.nextBabyId) body
                                                    in
                                                    applyInsertion newBody

                                                Core.After ->
                                                    let
                                                        newBody =
                                                            Document.Body.addElementAfterElement id (Document.Element.emptyBone model.nextBabyId) body
                                                    in
                                                    applyInsertion newBody

                                                Core.InsideFirst ->
                                                    let
                                                        newBody =
                                                            Document.Body.addElementInsideElementAsFirstChild id (Document.Element.emptyBone model.nextBabyId) body
                                                    in
                                                    applyInsertion newBody

                                                Core.InsideLast ->
                                                    let
                                                        newBody =
                                                            Document.Body.addElementInsideElementAsLastChild id (Document.Element.emptyBone model.nextBabyId) body
                                                    in
                                                    applyInsertion newBody

                                        Core.Flesh ->
                                            case additionType of
                                                Core.Before ->
                                                    let
                                                        newBody =
                                                            Document.Body.addElementBeforeElement id (Document.Element.emptyFlesh model.nextBabyId) body
                                                    in
                                                    applyInsertion newBody

                                                Core.After ->
                                                    let
                                                        newBody =
                                                            Document.Body.addElementAfterElement id (Document.Element.emptyFlesh model.nextBabyId) body
                                                    in
                                                    applyInsertion newBody

                                                _ ->
                                                    ( model, Cmd.none )

                                        _ ->
                                            ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
    in
    ( newModel, command )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Json.Decode.map2
                (KeyInteraction Down)
                Rest.keyDecoder
                Rest.shiftKeyDecoder
            )
        , documentToMarkupResult StartMarkupEditing
        , markupToDocumentResult RebuildDocument
        , updateMarkup UpdateMarkup
        ]
