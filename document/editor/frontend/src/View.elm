module View exposing (view)

import Core exposing (Model, Msg)
import Document.Html
import Html exposing (Html, button, div, h5, span, text, textarea)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Menu


markupModal : List (Html Msg)
markupModal =
    [ div [ class "modal-backdrop fade show" ] []
    , div
        [ class "modal fade show"
        , attribute "role" "dialog"
        , attribute "tabindex" "-1"
        , onClick (Core.SetMode Core.Default)
        ]
        [ div
            [ class "modal-dialog modal-dialog-centered"
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
                    [ h5 [ class "modal-title", id "exampleModalLongTitle" ]
                        [ text "Edit Markup" ]

                    -- Close button
                    , button
                        [ attribute "aria-label" "Close"
                        , class "close"
                        , attribute "data-dismiss" "modal"
                        , type_ "button"
                        , stopPropagationOn "click"
                            (Json.Decode.succeed
                                ( Core.SetMode Core.Default, True )
                            )
                        ]
                        [ span [ attribute "aria-hidden" "true" ]
                            [ text "×" ]
                        ]
                    ]
                , div [ class "modal-body" ]
                    -- Markup
                    [ textarea [ class "form-control" ] [] ]
                , div [ class "modal-footer" ]
                    -- Save button
                    [ button
                        [ class "btn btn-primary"
                        , type_ "button"
                        ]
                        [ text "Save changes" ]
                    ]
                ]
            ]
        ]
    ]


view : Model -> Html Msg
view model =
    div
        []
        (List.append
            [ Menu.toHtml model
            , div
                [ id "document"
                , class "container"
                ]
                [ case model.document.body of
                    Nothing ->
                        text "Document is blank, add elements to fill it up!"

                    Just body ->
                        body |> Document.Html.fromDocumentBody model
                ]
            ]
            (if model.mode == Core.MarkupEditing then
                markupModal

             else
                []
            )
        )
