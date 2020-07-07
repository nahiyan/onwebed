module View exposing (view)

import Core exposing (Model)
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
            [ text "Here is the content by Elm." ]
        ]
