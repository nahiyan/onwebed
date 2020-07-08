module Document.Html exposing (fromDocumentElement, fromTree)

import Core exposing (Mode(..), Model, Msg(..))
import Document.Element exposing (Element(..))
import Document.Elements.Tree
import Html exposing (Html, div, input, span, text, textarea)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (on, onBlur, onFocus, stopPropagationOn, targetValue)
import Json.Decode
import Tree
import Tree.Zipper


fromDocumentElement : Model -> Element -> List (Html Msg) -> Html Msg
fromDocumentElement model element children =
    case element of
        Root ->
            div [ Html.Attributes.id "elements" ] children

        Bone { id, descriptor, alternateHierarchy, selected } ->
            let
                attributes =
                    case model.mode of
                        ElementSelectionForRemoval ->
                            [ stopPropagationOn "click" (Json.Decode.succeed ( RemoveElement id, True ))
                            , stopPropagationOn "mousemove" (Json.Decode.succeed ( SelectElement id, True ))
                            ]

                        _ ->
                            []
            in
            div
                ([ class
                    ("bone"
                        ++ (if alternateHierarchy then
                                " alternate"

                            else
                                ""
                           )
                        ++ (if selected && not (List.member model.mode [ Default ]) then
                                " selected"

                            else
                                ""
                           )
                    )
                 , Html.Attributes.id ("element" ++ String.fromInt id)
                 ]
                    ++ attributes
                )
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
                        , on "input"
                            (Json.Decode.map (Core.SetBoneDescriptor id) targetValue)
                        , onFocus ToggleHotkeysEnabled
                        , onBlur ToggleHotkeysEnabled
                        ]
                        []
                    ]
                    :: children
                )

        Flesh { id, targets, content, selected } ->
            let
                attributes =
                    case model.mode of
                        ElementSelectionForRemoval ->
                            [ stopPropagationOn "click" (Json.Decode.succeed ( RemoveElement id, True ))
                            , stopPropagationOn "mousemove" (Json.Decode.succeed ( SelectElement id, True ))
                            ]

                        _ ->
                            []
            in
            div
                ([ class
                    ("flesh"
                        ++ (if selected && not (List.member model.mode [ Default ]) then
                                " selected"

                            else
                                ""
                           )
                    )
                 , Html.Attributes.id ("element" ++ String.fromInt id)
                 ]
                    ++ attributes
                )
                [ div
                    [ class "input-group" ]
                    [ div
                        [ class "input-group-prepend" ]
                        [ span [ class "input-group-text" ]
                            [ text "For" ]
                        ]
                    , input
                        [ type_ "text"
                        , value targets
                        , class "form-control"
                        , on "input"
                            (Json.Decode.map (Core.SetFleshTargets id) targetValue)
                        , onFocus ToggleHotkeysEnabled
                        , onBlur ToggleHotkeysEnabled
                        ]
                        []
                    ]
                , textarea
                    [ class "form-control"
                    , on "input"
                        (Json.Decode.map (Core.SetFleshContent id) targetValue)
                    , onFocus ToggleHotkeysEnabled
                    , onBlur ToggleHotkeysEnabled
                    ]
                    [ text content ]
                ]

        _ ->
            div
                []
                []


fromTree : Model -> Tree.Tree Element -> Html Msg
fromTree model tree =
    let
        treeWithMarkedAlternateHierarchy =
            Document.Elements.Tree.markAlternateHierarchy
                (Tree.Zipper.fromTree tree)
    in
    Tree.restructure (\element -> element) (fromDocumentElement model) treeWithMarkedAlternateHierarchy
