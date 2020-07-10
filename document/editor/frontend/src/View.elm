module View exposing (view)

import Core exposing (Model, Msg)
import Document.Html
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Menu


view : Model -> Html Msg
view model =
    div
        []
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
