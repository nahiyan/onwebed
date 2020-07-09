module Menu exposing (toHtml)

import Core exposing (Model, Msg(..))
import Html exposing (Html, a, button, div, h6, i, nav, span, text)
import Html.Attributes exposing (attribute, class, href, id)
import Html.Events exposing (onClick)



-- menu item


toHtml : Model -> Html Msg
toHtml model =
    let
        menuHeader =
            [ a
                [ href "../"
                , class "btn btn-outline-secondary"
                ]
                [ span
                    [ class "icon is-small" ]
                    [ i
                        [ class "fas fa-arrow-left" ]
                        []
                    ]
                , span
                    []
                    [ text "Back" ]
                ]
            , a
                [ attribute "href" "."
                , attribute "target" "__parent"
                , class "btn btn-outline-secondary"
                ]
                [ span
                    [ class "icon is-small" ]
                    [ i
                        [ class "fas fa-eye" ]
                        []
                    ]
                , span
                    []
                    [ text "View" ]
                ]
            , button
                [ class "btn btn-outline-success"

                -- , onClick Save
                , attribute "type" "button"
                ]
                [ span
                    [ class "icon is-small" ]
                    [ i
                        [ class "fas fa-save" ]
                        []
                    ]
                , span
                    []
                    [ text "Save" ]
                ]
            ]

        menuBody =
            [ div
                [ class "dropdown" ]
                [ button
                    [ class "btn btn-outline-success dropdown-toggle"
                    , id "addElement"
                    , Html.Attributes.attribute "type" "button"
                    , Html.Attributes.attribute "data-toggle" "dropdown"
                    , Html.Attributes.attribute "aria-haspopup" "true"
                    , Html.Attributes.attribute "aria-expanded" "false"
                    ]
                    [ text "+ Element" ]
                , div
                    [ class "dropdown-menu"
                    , Html.Attributes.attribute "aria-labelledby" "addElement"
                    ]
                    [ h6
                        [ class "dropdown-header" ]
                        [ text "Bone" ]
                    , span
                        [ class "dropdown-item"
                        , onClick (SetMode (Core.Selection Core.Bone (Core.Addition Core.Before)))
                        ]
                        [ text "Before" ]
                    , span
                        [ class "dropdown-item"
                        , onClick (SetMode (Core.Selection Core.Bone (Core.Addition Core.After)))
                        ]
                        [ text "After" ]
                    , span
                        [ class "dropdown-item"
                        , onClick (SetMode (Core.Selection Core.Bone (Core.Addition Core.InsideFirst)))
                        ]
                        [ text "Inside (First)" ]
                    , span
                        [ class "dropdown-item"
                        , onClick (SetMode (Core.Selection Core.Bone (Core.Addition Core.InsideLast)))
                        ]
                        [ text "Inside (Last)" ]
                    , div
                        [ class "dropdown-divider" ]
                        []
                    , Html.h1
                        [ class "dropdown-header" ]
                        [ text "Flesh" ]
                    , a
                        [ class "dropdown-item"
                        , Html.Attributes.attribute "href" "#"
                        ]
                        [ text "Before" ]
                    , a
                        [ class "dropdown-item"
                        , Html.Attributes.attribute "href" "#"
                        ]
                        [ text "After" ]
                    ]
                ]
            , button
                [ class "btn btn-outline-danger"
                , attribute "type" "button"
                , onClick (SetMode (Core.Selection Core.BoneAndFlesh Core.Removal))
                ]
                [ text "- Element"
                ]
            , button
                [ class "btn btn-outline-primary"

                -- , onClick Save
                , attribute "type" "button"
                ]
                [ text "Edit Markup"
                ]
            ]
    in
    nav
        [ id "menu"
        ]
        [ div
            [ class "container"
            ]
            menuHeader
        , div
            [ class "container"
            ]
            (case model.mode of
                Core.Selection _ Core.Removal ->
                    [ div []
                        [ text "Select an element which you want to remove." ]
                    ]

                Core.Selection _ (Core.Addition additionType) ->
                    [ div []
                        [ text
                            (case additionType of
                                Core.Before ->
                                    "Select an element before which you want to add a new element."

                                Core.After ->
                                    "Select an element after which you want to add a new element."

                                Core.InsideFirst ->
                                    "Select an element inside which you want to add a new element as its first child."

                                Core.InsideLast ->
                                    "Select an element inside which you want to add a new element as its last child."
                            )
                        ]
                    ]

                _ ->
                    menuBody
            )
        ]
