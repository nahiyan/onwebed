module Document.Html exposing (fromDocumentBody, fromDocumentElement)

import Core exposing (Mode, Model, Msg(..))
import Document.Body
import Document.Element exposing (Element(..))
import Html exposing (Html, div, input, span, text, textarea)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (on, onBlur, onFocus, stopPropagationOn, targetValue)
import Json.Decode
import Tree
import Tree.Zipper


isSelectionModeForBone : Mode -> Bool
isSelectionModeForBone mode =
    case mode of
        Core.Selection type_ _ ->
            List.member type_ [ Core.Bone, Core.BoneAndFlesh ]

        _ ->
            False


boneDisabledAttribute : Model -> Html.Attribute Msg
boneDisabledAttribute model =
    Html.Attributes.disabled
        (case model.mode of
            Core.Selection Core.Bone _ ->
                True

            Core.Selection Core.BoneAndFlesh _ ->
                True

            _ ->
                False
        )


fleshDisabledAttribute : Model -> Html.Attribute Msg
fleshDisabledAttribute model =
    Html.Attributes.disabled
        (case model.mode of
            Core.Selection Core.Flesh _ ->
                True

            Core.Selection Core.BoneAndFlesh _ ->
                True

            _ ->
                False
        )


isSelectionModeForFlesh : Mode -> Bool
isSelectionModeForFlesh mode =
    case mode of
        Core.Selection type_ _ ->
            List.member type_ [ Core.Flesh, Core.BoneAndFlesh ]

        _ ->
            False


fromDocumentElement : Model -> Element -> List (Html Msg) -> Html Msg
fromDocumentElement model element children =
    case element of
        Root ->
            div [ Html.Attributes.id "elements" ] children

        Bone { id, descriptor, alternateHierarchy, selected } ->
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
                                        Core.Selection Core.Flesh _ ->
                                            " no-selection"

                                        _ ->
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
                        , boneDisabledAttribute model
                        ]
                        []
                    ]
                    :: children
                )

        Flesh { id, targets, content, selected } ->
            let
                attributes =
                    if isSelectionModeForFlesh model.mode then
                        [ stopPropagationOn
                            "click"
                            (Json.Decode.succeed
                                ( ElementClick id, True )
                            )
                        , stopPropagationOn "mousemove" (Json.Decode.succeed ( SelectElement id, True ))
                        ]

                    else
                        []
            in
            div
                ([ class
                    ("flesh"
                        ++ (if selected && isSelectionModeForFlesh model.mode then
                                " selected"

                            else
                                ""
                           )
                        ++ (case model.mode of
                                Core.Selection Core.Bone _ ->
                                    " no-selection"

                                _ ->
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
                        , fleshDisabledAttribute model
                        ]
                        []
                    ]
                , textarea
                    [ class "form-control"
                    , on "input"
                        (Json.Decode.map (Core.SetFleshContent id) targetValue)
                    , onFocus ToggleHotkeysEnabled
                    , onBlur ToggleHotkeysEnabled
                    , fleshDisabledAttribute model
                    ]
                    [ text content ]
                ]

        _ ->
            div
                []
                []


fromDocumentBody : Model -> Tree.Tree Element -> Html Msg
fromDocumentBody model tree =
    let
        treeWithMarkedAlternateHierarchy =
            Document.Body.markAlternateHierarchy
                (Tree.Zipper.fromTree tree)
    in
    Tree.restructure (\element -> element) (fromDocumentElement model) treeWithMarkedAlternateHierarchy
