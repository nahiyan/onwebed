module View exposing (view)

import Core exposing (Model)
import Document.Elements.Tree
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Menu


view : Model -> Html msg
view model =
    div
        []
        [ Menu.toHtml model
        , div
            [ id "document"
            , class "container"
            ]
            [ case model.document of
                Nothing ->
                    text "Document is blank, add elements to fill it up!"

                Just tree ->
                    Document.Elements.Tree.toHtml tree
            ]
        ]
