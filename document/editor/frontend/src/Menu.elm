module Menu exposing (toHtml)

import Core exposing (Model, Msg(..))
import Html exposing (Html, a, button, div, h6, i, input, label, nav, span, text)
import Html.Attributes exposing (attribute, class, href, id, type_)
import Html.Events exposing (onClick)



-- menu item


toHtml : Model -> Html Msg
toHtml model =
    let
        saveButtonClasses =
            if model.saveState == Core.NoSaveRequired then
                " disabled"

            else
                ""

        saveButtonAttributes =
            if model.saveState == Core.SaveRequired then
                [ onClick Core.SaveDocument ]

            else
                []

        saveButtonContent =
            if model.saveState == Core.Saving then
                [ div
                    [ class "spinner-border spinner-border-sm mr-1"
                    , attribute "role" "status"
                    ]
                    [ span [ class "sr-only" ]
                        [ text "Loading..." ]
                    ]
                , span
                    []
                    [ text "Saving" ]
                ]

            else
                [ span
                    [ class "icon is-small" ]
                    [ i
                        [ class "fas fa-save" ]
                        []
                    ]
                , span [] [ text "Save" ]
                ]

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
                [ attribute "href" ("/view/" ++ model.fileName)
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
                (List.append
                    [ class ("btn btn-outline-success" ++ saveButtonClasses)
                    , attribute "type" "button"
                    , id "save-button"
                    ]
                    saveButtonAttributes
                )
                saveButtonContent
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
                    [ span
                        [ class "icon is-small" ]
                        [ i
                            [ class "fas fa-plus-circle" ]
                            []
                        ]
                    , span
                        []
                        [ text "Element" ]
                    ]
                , div
                    [ class "dropdown-menu"
                    , Html.Attributes.attribute "aria-labelledby" "addElement"
                    ]
                    [ h6
                        [ class "dropdown-header" ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-bone" ]
                                []
                            ]
                        , span
                            []
                            [ text "Bone" ]
                        ]
                    , span
                        [ class "dropdown-item"
                        , onClick
                            (SetMode
                                (Core.Selection
                                    Core.Bone
                                    (Core.Addition Core.Before)
                                )
                            )
                        ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-arrow-left" ]
                                []
                            ]
                        , span
                            []
                            [ text "Before" ]
                        ]
                    , span
                        [ class "dropdown-item"
                        , onClick
                            (SetMode
                                (Core.Selection
                                    Core.Bone
                                    (Core.Addition Core.After)
                                )
                            )
                        ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-arrow-right" ]
                                []
                            ]
                        , span
                            []
                            [ text "After" ]
                        ]
                    , span
                        [ class "dropdown-item"
                        , onClick
                            (SetMode
                                (Core.Selection
                                    Core.Bone
                                    (Core.Addition Core.InsideFirst)
                                )
                            )
                        ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-level-down-alt" ]
                                []
                            ]
                        , span
                            []
                            [ text "Inside (First)" ]
                        ]
                    , span
                        [ class "dropdown-item"
                        , onClick
                            (SetMode
                                (Core.Selection
                                    Core.Bone
                                    (Core.Addition Core.InsideLast)
                                )
                            )
                        ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-level-up-alt" ]
                                []
                            ]
                        , span
                            []
                            [ text "Inside (Last)" ]
                        ]
                    , div
                        [ class "dropdown-divider" ]
                        []
                    , h6
                        [ class "dropdown-header" ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-drumstick-bite" ]
                                []
                            ]
                        , span
                            []
                            [ text "Flesh" ]
                        ]
                    , span
                        [ class "dropdown-item"
                        , onClick
                            (SetMode
                                (Core.Selection
                                    Core.Flesh
                                    (Core.Addition Core.Before)
                                )
                            )
                        ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-arrow-left" ]
                                []
                            ]
                        , span
                            []
                            [ text "Before" ]
                        ]
                    , span
                        [ class "dropdown-item"
                        , onClick
                            (SetMode
                                (Core.Selection
                                    Core.Flesh
                                    (Core.Addition Core.After)
                                )
                            )
                        ]
                        [ span
                            [ class "icon is-small" ]
                            [ i
                                [ class "fas fa-arrow-right" ]
                                []
                            ]
                        , span
                            []
                            [ text "After" ]
                        ]
                    ]
                ]
            , button
                [ class "btn btn-outline-danger"
                , attribute "type" "button"
                , onClick (SetMode (Core.Selection Core.All Core.Removal))
                ]
                [ span
                    [ class "icon is-small" ]
                    [ i
                        [ class "fas fa-minus-circle" ]
                        []
                    ]
                , span
                    []
                    [ text "Element" ]
                ]
            , button
                [ class "btn btn-outline-primary"
                , attribute "type" "button"
                , onClick Core.PrepareMarkupEditing
                ]
                [ span
                    [ class "icon is-small" ]
                    [ i
                        [ class "fas fa-code" ]
                        []
                    ]
                , span
                    []
                    [ text "Edit Markup" ]
                ]
            , div [ class "btn-group btn-group-toggle", attribute "data-toggle" "buttons" ]
                [ label
                    [ class
                        ("btn btn-outline-primary"
                            ++ (if model.filter == Core.All then
                                    " active"

                                else
                                    ""
                               )
                        )
                    ]
                    [ input
                        (List.append
                            (if model.filter == Core.All then
                                [ attribute "checked" "" ]

                             else
                                []
                            )
                            [ type_ "radio"
                            , onClick (Core.SetFilter Core.All)
                            ]
                        )
                        []
                    , span
                        [ class "icon is-small" ]
                        [ i
                            [ class "fas fa-check-double" ]
                            []
                        ]
                    , span
                        []
                        [ text "All" ]
                    ]
                , label
                    [ class
                        ("btn btn-outline-primary"
                            ++ (if model.filter == Core.Bone then
                                    " active"

                                else
                                    ""
                               )
                        )
                    ]
                    [ input
                        (List.append
                            (if model.filter == Core.Bone then
                                [ attribute "checked" "" ]

                             else
                                []
                            )
                            [ type_ "radio"
                            , onClick (Core.SetFilter Core.Bone)
                            ]
                        )
                        []
                    , span
                        [ class "icon is-small" ]
                        [ i
                            [ class "fas fa-bone" ]
                            []
                        ]
                    , span
                        []
                        [ text "Bone" ]
                    ]
                , label
                    [ class
                        ("btn btn-outline-primary"
                            ++ (if model.filter == Core.Flesh then
                                    " active"

                                else
                                    ""
                               )
                        )
                    ]
                    [ input
                        (List.append
                            (if model.filter == Core.Flesh then
                                [ attribute "checked" "" ]

                             else
                                []
                            )
                            [ type_ "radio"
                            , onClick (Core.SetFilter Core.Flesh)
                            ]
                        )
                        []
                    , span
                        [ class "icon is-small" ]
                        [ i
                            [ class "fas fa-drumstick-bite" ]
                            []
                        ]
                    , span
                        []
                        [ text "Flesh" ]
                    ]
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
                Core.Selection _ _ ->
                    [ div
                        []
                        [ case model.mode of
                            Core.Selection _ Core.Removal ->
                                text "Select an element which you want to remove."

                            Core.Selection _ (Core.Addition additionType) ->
                                text
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

                            _ ->
                                text ""
                        ]
                    ]

                _ ->
                    menuBody
            )
        ]
