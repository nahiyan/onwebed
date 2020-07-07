module Document.Html exposing (fromDocumentElement, fromTree)

import Core exposing (Msg)
import Document.Element exposing (Element(..))
import Document.Elements.Tree
import Html exposing (Html, div, input, span, text, textarea)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (on, targetValue)
import Json.Decode
import Tree
import Tree.Zipper


fromDocumentElement : Element -> List (Html Msg) -> Html Msg
fromDocumentElement element children =
    case element of
        Root ->
            div [ Html.Attributes.id "elements" ] children

        Bone { id, descriptor, alternateHierarchy } ->
            div
                [ class
                    ("bone"
                        ++ (if alternateHierarchy then
                                " alternate"

                            else
                                ""
                           )
                    )
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
                        , on "input"
                            (Json.Decode.map (Core.SetBoneDescriptor id) targetValue)
                        ]
                        []
                    ]
                    :: children
                )

        Flesh { id, targets, content } ->
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
                        , value targets
                        , class "form-control"
                        , on "input"
                            (Json.Decode.map (Core.SetFleshTargets id) targetValue)
                        ]
                        []
                    ]
                , textarea
                    [ class "form-control"
                    , on "input"
                        (Json.Decode.map (Core.SetFleshContent id) targetValue)
                    ]
                    [ text content ]
                ]

        _ ->
            div
                []
                []


fromTree : Tree.Tree Element -> Html Msg
fromTree tree =
    let
        treeWithMarkedAlternateHierarchy =
            Document.Elements.Tree.markAlternateHierarchy
                (Tree.Zipper.fromTree tree)
    in
    Tree.restructure (\element -> element) fromDocumentElement treeWithMarkedAlternateHierarchy
