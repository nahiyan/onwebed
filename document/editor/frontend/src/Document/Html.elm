module Document.Html exposing (fromDocumentElement, fromTree)

import Core exposing (Mode, Model, Msg(..))
import Document.Element exposing (Element(..))
import Document.Elements.Tree
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
                    if isSelectionModeForBone model.mode then
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
