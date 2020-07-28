module Document.Html exposing (fromDocumentBody, fromDocumentElement)

import Core exposing (Mode, Model, Msg(..))
import Document.Body
import Document.Element exposing (Element(..), SelectionType(..))
import Html exposing (Html, div, i, input, span, text, textarea)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (on, onBlur, onFocus, stopPropagationOn, targetValue)
import Json.Decode
import Tree
import Tree.Zipper


isSelectionModeForBone : Mode -> Bool
isSelectionModeForBone mode =
    case mode of
        Core.Selection type_ _ ->
            List.member type_ [ Document.Element.Bones, Document.Element.All ]

        _ ->
            False


boneDisabledAttribute : Model -> Html.Attribute Msg
boneDisabledAttribute model =
    Html.Attributes.disabled
        (case model.mode of
            Core.Selection Document.Element.Bones _ ->
                True

            Core.Selection Document.Element.All _ ->
                True

            _ ->
                False
        )


fleshDisabledAttribute : Model -> Html.Attribute Msg
fleshDisabledAttribute model =
    Html.Attributes.disabled
        (case model.mode of
            Core.Selection Document.Element.FleshItems _ ->
                True

            Core.Selection Document.Element.All _ ->
                True

            _ ->
                False
        )


isSelectionModeForFlesh : Mode -> Bool
isSelectionModeForFlesh mode =
    case mode of
        Core.Selection type_ _ ->
            List.member type_ [ Document.Element.FleshItems, Document.Element.All ]

        _ ->
            False


fromBone : Element -> List (Html Msg) -> Model -> Html Msg
fromBone bone children model =
    case bone of
        Bone { id, descriptor, alternateHierarchy, selected, babyId } ->
            let
                attributes =
                    List.append
                        [ class
                            ("bone"
                                ++ (if alternateHierarchy then
                                        " alternate"

                                    else
                                        ""
                                   )
                                ++ (if selected && isSelectionModeForBone model.mode then
                                        " selected"

                                    else
                                        ""
                                   )
                                ++ (case model.mode of
                                        Core.Selection Document.Element.FleshItems _ ->
                                            " no-selection"

                                        _ ->
                                            ""
                                   )
                                ++ (if model.filter == Document.Element.FleshItems then
                                        " hide"

                                    else
                                        ""
                                   )
                                ++ (case babyId of
                                        Just _ ->
                                            " baby"

                                        Nothing ->
                                            ""
                                   )
                            )
                        , Html.Attributes.id ("element" ++ String.fromInt id)
                        ]
                        (if isSelectionModeForBone model.mode then
                            [ stopPropagationOn
                                "click"
                                (Json.Decode.succeed
                                    ( ElementClick id, True )
                                )
                            , stopPropagationOn "mousemove" (Json.Decode.succeed ( SelectElement id, True ))
                            ]

                         else
                            []
                        )
            in
            div
                attributes
                (div
                    [ class "input-group" ]
                    [ div
                        [ class "input-group-prepend" ]
                        [ div [ class "input-group-text" ]
                            [ span
                                [ class "icon is-small" ]
                                [ i
                                    [ class "fas fa-book mr-2" ]
                                    []
                                ]
                            , span
                                []
                                [ text "Descriptor" ]
                            ]
                        ]
                    , input
                        [ type_ "text"
                        , value descriptor
                        , class "form-control"
                        , on "input"
                            (Json.Decode.map (Core.SetElementProperty Document.Element.Bones id "descriptor") targetValue)
                        , onFocus ToggleHotkeysEnabled
                        , onBlur ToggleHotkeysEnabled
                        , boneDisabledAttribute model
                        ]
                        []
                    ]
                    :: children
                )

        _ ->
            div [] []


fromFlesh : Element -> Model -> Html Msg
fromFlesh flesh model =
    case flesh of
        Flesh { id, for, attributes, content, selected, babyId } ->
            let
                attributes_ =
                    List.append
                        [ class
                            ("flesh"
                                ++ (if selected && isSelectionModeForFlesh model.mode then
                                        " selected"

                                    else
                                        ""
                                   )
                                ++ (case model.mode of
                                        Core.Selection Document.Element.Bones _ ->
                                            " no-selection"

                                        _ ->
                                            ""
                                   )
                                ++ (if model.filter == Document.Element.Bones then
                                        " hide"

                                    else
                                        ""
                                   )
                                ++ (case babyId of
                                        Just _ ->
                                            " baby"

                                        Nothing ->
                                            ""
                                   )
                            )
                        , Html.Attributes.id ("element" ++ String.fromInt id)
                        ]
                        (if isSelectionModeForFlesh model.mode then
                            [ stopPropagationOn
                                "click"
                                (Json.Decode.succeed
                                    ( ElementClick id, True )
                                )
                            , stopPropagationOn "mousemove" (Json.Decode.succeed ( SelectElement id, True ))
                            ]

                         else
                            []
                        )
            in
            div
                attributes_
                [ div
                    [ class "row w-100" ]
                    [ div
                        [ class "col-md input-group" ]
                        [ div
                            [ class "input-group-prepend" ]
                            [ div
                                [ class "input-group-text" ]
                                [ span
                                    []
                                    [ span
                                        [ class "icon is-small" ]
                                        [ i
                                            [ class "fas fa-bullseye mr-2" ]
                                            []
                                        ]
                                    , span
                                        []
                                        [ text "Targets" ]
                                    ]
                                ]
                            ]
                        , input
                            [ type_ "text"
                            , value for
                            , class "form-control"
                            , on "input"
                                (Json.Decode.map (Core.SetElementProperty Document.Element.FleshItems id "for") targetValue)
                            , onFocus ToggleHotkeysEnabled
                            , onBlur ToggleHotkeysEnabled
                            , fleshDisabledAttribute model
                            ]
                            []
                        ]
                    , div
                        [ class "col-md input-group" ]
                        [ div
                            [ class "input-group-prepend" ]
                            [ div
                                [ class "input-group-text" ]
                                [ span
                                    []
                                    [ span
                                        [ class "icon is-small" ]
                                        [ i
                                            [ class "fas fa-sliders-h mr-2" ]
                                            []
                                        ]
                                    , span
                                        []
                                        [ text "Attributes" ]
                                    ]
                                ]
                            ]
                        , input
                            [ type_ "text"
                            , value attributes
                            , class "form-control"
                            , on "input"
                                (Json.Decode.map (Core.SetElementProperty Document.Element.FleshItems id "attributes") targetValue)
                            , onFocus ToggleHotkeysEnabled
                            , onBlur ToggleHotkeysEnabled
                            , fleshDisabledAttribute model
                            ]
                            []
                        ]
                    ]
                , textarea
                    [ class "form-control"
                    , on "input"
                        (Json.Decode.map (Core.SetElementProperty Document.Element.FleshItems id "content") targetValue)
                    , onFocus ToggleHotkeysEnabled
                    , onBlur ToggleHotkeysEnabled
                    , fleshDisabledAttribute model
                    , value content
                    ]
                    []
                ]

        _ ->
            div [] []


fromDocumentElement : Model -> Element -> List (Html Msg) -> Html Msg
fromDocumentElement model element children =
    case element of
        Root ->
            div [ id "elements" ] children

        Bone bone ->
            fromBone (Bone bone) children model

        Flesh flesh ->
            fromFlesh (Flesh flesh) model

        _ ->
            div [] []


fromDocumentBody : Model -> Tree.Tree Element -> Html Msg
fromDocumentBody model tree =
    let
        treeWithMarkedAlternateHierarchy =
            Document.Body.markAlternateHierarchy
                (Tree.Zipper.fromTree tree)
    in
    Tree.restructure identity (fromDocumentElement model) treeWithMarkedAlternateHierarchy
