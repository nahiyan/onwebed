module Menu exposing (toHtml)

import Core exposing (Model, Msg(..))
import Document
import Document.Element
import Html exposing (Html, a, button, div, form, h6, i, input, label, nav, span, text)
import Html.Attributes exposing (attribute, checked, class, for, href, id, name, type_)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)



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
                    [ class "spinner-border spinner-border-sm me-1"
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
                [ attribute "href" ("/view/" ++ model.machineName)
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
            , form
                [ class "form-inline" ]
                [ div
                    [ class "input-group" ]
                    [ div
                        [ class "input-group-prepend" ]
                        [ div
                            [ class "input-group-text" ]
                            [ span
                                [ class "icon is-small" ]
                                [ i
                                    [ class "fas fa-id-badge" ]
                                    []
                                ]
                            , text "Name"
                            ]
                        ]
                    , input
                        [ class "form-control"
                        , id "document-name"
                        , Html.Attributes.attribute "value" (Maybe.withDefault "" model.document.name)
                        , onInput SetDocumentName
                        , onFocus ToggleHotkeysEnabled
                        , onBlur ToggleHotkeysEnabled
                        ]
                        []
                    ]
                ]
            ]

        addElementDropdownButton =
            let
                elementsCount =
                    Document.elementsCount model.document

                boneAdditionOptions =
                    if elementsCount.bone > 0 then
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
                                        Document.Element.Bones
                                        (Document.Element.Addition Document.Element.Before)
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
                                        Document.Element.Bones
                                        (Document.Element.Addition Document.Element.After)
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
                                        Document.Element.Bones
                                        (Document.Element.Addition Document.Element.InsideFirst)
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
                                        Document.Element.Bones
                                        (Document.Element.Addition Document.Element.InsideLast)
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
                        ]

                    else
                        [ span
                            [ class "dropdown-item"
                            , onClick (AddElement Document.Element.Bones Document.Element.First)
                            ]
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
                        ]

                fleshAdditionOptions =
                    if elementsCount.flesh > 0 then
                        [ h6
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
                                        Document.Element.FleshItems
                                        (Document.Element.Addition Document.Element.Before)
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
                                        Document.Element.FleshItems
                                        (Document.Element.Addition Document.Element.After)
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

                    else
                        [ span
                            [ class "dropdown-item"
                            , onClick (AddElement Document.Element.FleshItems Document.Element.Last)
                            ]
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
                        ]
            in
            div
                [ class "dropdown" ]
                [ button
                    [ class "btn btn-outline-success dropdown-toggle"
                    , id "addElement"
                    , type_ "button"
                    , attribute "data-bs-toggle" "dropdown"
                    , attribute "aria-expanded" "false"
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
                    (List.append
                        (List.append
                            boneAdditionOptions
                            (if elementsCount.bone > 0 || elementsCount.flesh > 0 then
                                [ div
                                    [ class "dropdown-divider" ]
                                    []
                                ]

                             else
                                []
                            )
                        )
                        fleshAdditionOptions
                    )
                ]

        menuBody =
            [ addElementDropdownButton
            , button
                [ class "btn btn-outline-danger"
                , attribute "type" "button"
                , onClick (SetMode (Core.Selection Document.Element.All Document.Element.Removal))
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
            , div [ class "btn-group", attribute "role" "group", attribute "area-label" "Basic radio toggle button group" ]
                [ input
                    [ id "filter-all"
                    , class "btn-check"
                    , type_ "radio"
                    , name "filter"
                    , attribute "autocomplete" "off"
                    , onClick (Core.SetFilter Document.Element.All)
                    , if model.filter == Document.Element.All then
                        checked True

                      else
                        checked False
                    ]
                    []
                , label [ class "btn btn-outline-primary", for "filter-all" ]
                    [ span
                        [ class "icon is-small" ]
                        [ i
                            [ class "fas fa-check-double me-2" ]
                            []
                        , text "All"
                        ]
                    ]
                , input
                    [ id "filter-bone"
                    , class "btn-check"
                    , type_ "radio"
                    , name "filter"
                    , attribute "autocomplete" "off"
                    , onClick (Core.SetFilter Document.Element.Bones)
                    , if model.filter == Document.Element.Bones then
                        checked True

                      else
                        checked False
                    ]
                    []
                , label [ class "btn btn-outline-primary", for "filter-bone" ]
                    [ span
                        [ class "icon is-small" ]
                        [ i
                            [ class "fas fa-bone me-2" ]
                            []
                        , text "Bone"
                        ]
                    ]
                , input
                    [ id "filter-flesh"
                    , class "btn-check"
                    , type_ "radio"
                    , name "filter"
                    , attribute "autocomplete" "off"
                    , onClick (Core.SetFilter Document.Element.FleshItems)
                    , if model.filter == Document.Element.FleshItems then
                        checked True

                      else
                        checked False
                    ]
                    []
                , label [ class "btn btn-outline-primary", for "filter-flesh" ]
                    [ span
                        [ class "icon is-small" ]
                        [ i
                            [ class "fas fa-drumstick-bite me-2" ]
                            []
                        , text "Flesh"
                        ]
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
                            Core.Selection _ Document.Element.Removal ->
                                text "Select an element that you want to remove."

                            Core.Selection _ (Document.Element.Addition additionType) ->
                                text
                                    (case additionType of
                                        Document.Element.Before ->
                                            "Select an element before that you want to add a new element."

                                        Document.Element.After ->
                                            "Select an element after that you want to add a new element."

                                        Document.Element.InsideFirst ->
                                            "Select an element inside that you want to add a new element as its first child."

                                        Document.Element.InsideLast ->
                                            "Select an element inside that you want to add a new element as its last child."

                                        _ ->
                                            ""
                                    )

                            _ ->
                                text ""
                        ]
                    ]

                _ ->
                    menuBody
            )
        ]
