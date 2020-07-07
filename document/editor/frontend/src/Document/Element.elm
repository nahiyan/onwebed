module Document.Element exposing (Element(..), toHtml)

import Html exposing (Html, div, input, span, text, textarea)
import Html.Attributes exposing (class, type_, value)


type Element
    = Bone { id : Int, descriptor : String }
    | Flesh { id : Int, for : String, content : String }
    | Text String
    | Root


toHtml : Element -> List (Html msg) -> Html msg
toHtml element children =
    case element of
        Root ->
            div [ Html.Attributes.id "elements" ] children

        Bone { id, descriptor } ->
            div
                [ class "bone"
                , Html.Attributes.id ("element" ++ String.fromInt id)
                ]
                (div
                    [ class "input-group" ]
                    [ div
                        [ class "input-group-prepend"
                        ]
                        [ span [ class "input-group-text" ]
                            [ text "Descriptor" ]
                        ]
                    , input
                        [ type_ "text"
                        , value descriptor
                        , class "form-control"
                        ]
                        []
                    ]
                    :: children
                )

        Flesh { id, for, content } ->
            div
                [ class "flesh"
                , Html.Attributes.id ("element" ++ String.fromInt id)
                ]
                [ div
                    [ class "input-group" ]
                    [ div
                        [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ]
                            [ text "For" ]
                        ]
                    , input
                        [ type_ "text"
                        , value for
                        , class "form-control"
                        ]
                        []
                    ]
                , textarea
                    [ class "form-control" ]
                    [ text content ]
                ]

        _ ->
            div
                []
                []
