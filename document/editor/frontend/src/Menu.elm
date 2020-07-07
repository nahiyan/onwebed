module Menu exposing (toHtml)

import Core exposing (Model)
import Html exposing (Html, a, button, div, i, input, nav, span, text)
import Html.Attributes exposing (attribute, class, href, id, type_)


type alias MenuItem =
    { name : String
    , machineName : String
    }


menuItem : String -> String -> MenuItem
menuItem name machineName =
    { name = name
    , machineName = machineName
    }



-- menu item


menuItemToHtml : MenuItem -> Html msg
menuItemToHtml item =
    div
        []
        [ input
            [ type_ "button"
            , Html.Attributes.value item.name
            , class "btn btn-outline-primary"

            -- , onClick (MenuItemClick item.machineName)
            ]
            []
        ]


menuItemsToHtml : List MenuItem -> List (Html msg)
menuItemsToHtml menuItemsToBeConverted =
    List.map menuItemToHtml menuItemsToBeConverted


toHtml : Model -> Html msg
toHtml _ =
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
            [ menuItem "+ Element" "add_element"
            , menuItem "- Element" "remove_element"
            , menuItem "Edit Markup" "edit_markup"
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
            (menuItemsToHtml menuBody)
        ]
