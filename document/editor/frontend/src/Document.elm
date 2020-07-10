module Document exposing (Document, fromString, toJson)

import Dict
import Document.Body
import Document.Element exposing (Element)
import Json.Decode
import Json.Encode
import Tree exposing (Tree)
import Tree.Zipper


type alias Document =
    { name : Maybe String
    , body : Maybe (Tree Element)
    }


combineTextElements : Maybe (List (Tree Element)) -> String
combineTextElements elements =
    List.foldl
        (\elementTree acc ->
            case Tree.label elementTree of
                Document.Element.Text text_ ->
                    acc ++ text_

                _ ->
                    ""
        )
        ""
        (Maybe.withDefault [] elements)


decoder : Json.Decode.Decoder (List (Tree Element))
decoder =
    Json.Decode.list
        (Json.Decode.map5
            (\type_ name elements attributes text ->
                case type_ of
                    "element" ->
                        case Maybe.withDefault "" name of
                            "bone" ->
                                let
                                    descriptor =
                                        case attributes of
                                            Nothing ->
                                                ""

                                            Just justAttributes ->
                                                case Dict.get "descriptor" justAttributes of
                                                    Nothing ->
                                                        ""

                                                    Just justDescriptor ->
                                                        justDescriptor
                                in
                                Tree.tree (Document.Element.Bone { id = 0, descriptor = descriptor, alternateHierarchy = False, selected = False })
                                    (Maybe.withDefault [] elements)

                            "document" ->
                                Tree.tree Document.Element.Root (Maybe.withDefault [] elements)

                            "name" ->
                                Tree.tree (Document.Element.Name (combineTextElements elements)) []

                            "head" ->
                                Tree.tree Document.Element.Head (Maybe.withDefault [] elements)

                            "body" ->
                                Tree.tree Document.Element.Body (Maybe.withDefault [] elements)

                            -- Flesh
                            _ ->
                                let
                                    targets =
                                        case attributes of
                                            Nothing ->
                                                ""

                                            Just justAttributes ->
                                                case Dict.get "for" justAttributes of
                                                    Nothing ->
                                                        ""

                                                    Just justTarget ->
                                                        justTarget

                                    content =
                                        combineTextElements elements
                                in
                                Tree.tree (Document.Element.Flesh { id = 0, targets = targets, content = content, selected = False }) []

                    -- Text
                    _ ->
                        Tree.tree (Document.Element.Text (Maybe.withDefault "" text)) []
            )
            (Json.Decode.field "type" Json.Decode.string)
            (Json.Decode.maybe
                (Json.Decode.field
                    "name"
                    Json.Decode.string
                )
            )
            (Json.Decode.maybe
                (Json.Decode.field
                    "elements"
                    (Json.Decode.lazy (\_ -> decoder))
                )
            )
            (Json.Decode.maybe
                (Json.Decode.field
                    "attributes"
                    (Json.Decode.dict Json.Decode.string)
                )
            )
            (Json.Decode.maybe
                (Json.Decode.field
                    "text"
                    Json.Decode.string
                )
            )
        )


fromString : String -> Document
fromString jsonString =
    case Json.Decode.decodeString (Json.Decode.field "elements" decoder) jsonString of
        Ok elements ->
            let
                zipper =
                    case elements of
                        [ root ] ->
                            Tree.Zipper.fromTree root

                        _ ->
                            Tree.Zipper.fromTree (Tree.singleton Document.Element.Root)

                name =
                    (zipper
                        |> Tree.Zipper.findFromRoot
                            (\element ->
                                case element of
                                    Document.Element.Head ->
                                        True

                                    _ ->
                                        False
                            )
                    )
                        |> Maybe.andThen
                            (\headZipper ->
                                let
                                    maybeNameZipper =
                                        headZipper
                                            |> Tree.Zipper.findFromRoot
                                                (\element ->
                                                    case element of
                                                        Document.Element.Name _ ->
                                                            True

                                                        _ ->
                                                            False
                                                )
                                in
                                case maybeNameZipper of
                                    Just justNameZipper ->
                                        case justNameZipper |> Tree.Zipper.label of
                                            Document.Element.Name justName ->
                                                Just justName

                                            _ ->
                                                Nothing

                                    Nothing ->
                                        Nothing
                            )

                body =
                    zipper
                        |> Tree.Zipper.findFromRoot
                            (\element ->
                                case element of
                                    Document.Element.Body ->
                                        True

                                    _ ->
                                        False
                            )
                        |> Maybe.andThen
                            (\bodyZipper ->
                                let
                                    bodyChildren =
                                        bodyZipper |> Tree.Zipper.children
                                in
                                Just (Tree.tree Document.Element.Root bodyChildren |> Document.Body.applyIndex)
                            )
            in
            { name = name, body = body }

        Err _ ->
            { name = Nothing, body = Nothing }


toJson : Document -> String
toJson document =
    let
        xmlElement =
            \name attributes children ->
                [ ( "type", Json.Encode.string "element" )
                , ( "name", Json.Encode.string name )
                , ( "elements", Json.Encode.list Json.Encode.object children )
                ]
                    ++ (if not (List.isEmpty attributes) then
                            [ ( "attributes", Json.Encode.object attributes ) ]

                        else
                            []
                       )

        xmlTextElement =
            \text ->
                [ ( "type", Json.Encode.string "text" )
                , ( "text", Json.Encode.string text )
                ]

        headElement =
            xmlElement "head"
                []
                ([]
                    ++ (case document.name of
                            Just name ->
                                [ xmlElement "name" [] [ xmlTextElement name ] ]

                            Nothing ->
                                []
                       )
                )

        bodyElement =
            case document.body of
                Nothing ->
                    []

                Just tree ->
                    Tree.restructure
                        (\element -> element)
                        (\element children ->
                            case element of
                                Document.Element.Bone { descriptor } ->
                                    xmlElement "bone" [ ( "descriptor", Json.Encode.string descriptor ) ] children

                                Document.Element.Flesh { targets, content } ->
                                    xmlElement "flesh" [ ( "for", Json.Encode.string targets ) ] [ xmlTextElement content ]

                                Document.Element.Root ->
                                    xmlElement "body" [] children

                                _ ->
                                    xmlElement "" [] []
                        )
                        tree

        documentElement =
            xmlElement "document" [] [ headElement, bodyElement ]

        root =
            Json.Encode.object
                [ ( "elements", Json.Encode.list Json.Encode.object [ documentElement ] )
                ]
    in
    Json.Encode.encode 0 root
