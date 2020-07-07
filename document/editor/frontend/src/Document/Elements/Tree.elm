module Document.Elements.Tree exposing (fromString, markAlternateHierarchy, replaceElement)

import Dict
import Document.Element exposing (Element(..))
import Json.Decode exposing (Decoder, decodeString, dict, field, lazy, list, map5, maybe, string)
import Tree exposing (Tree, label, tree)
import Tree.Zipper


decoder : Decoder (List (Tree Element))
decoder =
    list
        (map5
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
                                tree (Bone { id = 0, descriptor = descriptor, alternateHierarchy = False })
                                    (Maybe.withDefault [] elements)

                            "document_body" ->
                                tree Root (Maybe.withDefault [] elements)

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
                                        List.foldl
                                            (\elementTree acc ->
                                                case label elementTree of
                                                    Text text_ ->
                                                        acc ++ text_

                                                    _ ->
                                                        ""
                                            )
                                            ""
                                            (Maybe.withDefault [] elements)
                                in
                                tree (Flesh { id = 0, targets = targets, content = content }) []

                    -- Text
                    _ ->
                        tree (Text (Maybe.withDefault "" text)) []
            )
            (field "type" string)
            (maybe (field "name" string))
            (maybe (field "elements" (lazy (\_ -> decoder))))
            (maybe (field "attributes" (dict string)))
            (maybe (field "text" string))
        )


fromString : String -> Maybe (Tree Element)
fromString jsonString =
    case decodeString (field "elements" decoder) jsonString of
        Ok elements ->
            let
                tree_ =
                    tree Root elements

                indexedTree =
                    Tree.indexedMap
                        (\index element ->
                            case element of
                                Bone bone ->
                                    Bone { bone | id = index }

                                Flesh flesh ->
                                    Flesh { flesh | id = index }

                                _ ->
                                    element
                        )
                        tree_
            in
            Just indexedTree

        Err _ ->
            Nothing


markAlternateHierarchy : Tree.Zipper.Zipper Element -> Tree Element
markAlternateHierarchy zipper =
    let
        newZipper =
            case Tree.Zipper.label zipper of
                Bone currentBone ->
                    case Tree.Zipper.parent zipper of
                        Just parentZipper ->
                            let
                                parent =
                                    parentZipper |> Tree.Zipper.label
                            in
                            case parent of
                                Bone parentBone ->
                                    zipper |> Tree.Zipper.replaceLabel (Bone { currentBone | alternateHierarchy = not parentBone.alternateHierarchy }) |> Tree.Zipper.forward

                                _ ->
                                    Tree.Zipper.forward zipper

                        _ ->
                            Tree.Zipper.forward zipper

                _ ->
                    Tree.Zipper.forward zipper
    in
    case newZipper of
        Nothing ->
            Tree.Zipper.toTree zipper

        Just justNewZipper ->
            markAlternateHierarchy justNewZipper


replaceElementStep : Int -> (Element -> Element) -> Tree.Zipper.Zipper Element -> Tree Element
replaceElementStep index replace zipper =
    let
        currentElement =
            zipper |> Tree.Zipper.label

        isDesiredElement =
            case currentElement of
                Bone bone ->
                    bone.id == index

                Flesh flesh ->
                    flesh.id == index

                _ ->
                    False
    in
    if isDesiredElement then
        let
            replacement =
                replace currentElement
        in
        zipper
            |> Tree.Zipper.replaceLabel replacement
            |> Tree.Zipper.toTree

    else
        let
            newZipper =
                zipper |> Tree.Zipper.forward
        in
        case newZipper of
            Nothing ->
                Tree.Zipper.toTree zipper

            Just justNewZipper ->
                replaceElementStep index replace justNewZipper


replaceElement : Int -> (Element -> Element) -> Tree Element -> Tree Element
replaceElement index replace tree =
    let
        zipper =
            Tree.Zipper.fromTree tree
    in
    replaceElementStep index replace zipper
