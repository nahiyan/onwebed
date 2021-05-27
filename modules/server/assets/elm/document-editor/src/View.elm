module View exposing (view)

import Browser
import Core exposing (Model, Msg)
import Document
import Document.Html
import Html exposing (button, div, h5, i, span, text)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Menu


view : Model -> Browser.Document Msg
view model =
    let
        markupModalClasses =
            if model.mode == Core.MarkupEditing then
                " show"

            else
                " hide"

        markupModal =
            [ div
                [ class ("modal-backdrop fade" ++ markupModalClasses) ]
                []
            , div
                [ class ("modal fade" ++ markupModalClasses)
                , attribute "role" "dialog"
                , attribute "tabindex" "-1"
                , onClick Core.EndMarkupEditing
                ]
                [ div
                    [ class "modal-dialog modal-dialog-centered modal-lg"
                    , attribute "role" "document"
                    ]
                    [ div
                        [ class "modal-content"
                        , stopPropagationOn "click"
                            (Json.Decode.succeed
                                ( Core.SetMode Core.MarkupEditing, True )
                            )
                        ]
                        [ div [ class "modal-header" ]
                            -- Title
                            [ h5 [ class "modal-title" ]
                                [ text "Edit Markup" ]

                            -- Close button
                            , button
                                [ attribute "aria-label" "Close"
                                , class "btn-close"
                                , attribute "data-dismiss" "modal"
                                , type_ "button"
                                , stopPropagationOn "click"
                                    (Json.Decode.succeed
                                        ( Core.EndMarkupEditing, True )
                                    )
                                ]
                                []
                            ]
                        , div [ class "modal-body" ]
                            -- Markup
                            [ div [ id "markup-editor" ]
                                [ text "" ]
                            ]
                        , div [ class "modal-footer" ]
                            -- Save button
                            [ button
                                [ class "btn btn-outline-success"
                                , type_ "button"
                                , stopPropagationOn "click"
                                    (Json.Decode.succeed
                                        ( Core.ApplyMarkup, True )
                                    )
                                ]
                                [ span
                                    [ class "icon is-small me-2" ]
                                    [ i
                                        [ class "fas fa-save" ]
                                        []
                                    ]
                                , span
                                    []
                                    [ text "Save Changes" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Onwebed - Document Editor"
    , body =
        List.append
            [ Menu.toHtml model
            , div
                [ id "document"
                , class "container"
                ]
                [ case model.document.body of
                    Nothing ->
                        text "Document is blank, add elements to fill it up!"

                    Just body ->
                        if Document.isEmpty model.document then
                            text "Document is blank, add elements to fill it up!"

                        else
                            body |> Document.Html.fromDocumentBody model
                ]
            ]
            markupModal
    }
