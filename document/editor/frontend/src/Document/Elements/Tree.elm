module Document.Elements.Tree exposing (fromString, toHtml)

import Dict
import Document.Element exposing (Element(..))
import Html exposing (Html, text)
import Json.Decode exposing (Decoder, decodeString, dict, field, lazy, list, map5, maybe, string)
import Tree exposing (Tree, label, tree)


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
                                tree (Bone { id = 0, descriptor = descriptor })
                                    (Maybe.withDefault [] elements)

                            "document_body" ->
                                tree Root (Maybe.withDefault [] elements)

                            -- Flesh
                            _ ->
                                let
                                    for =
                                        case attributes of
                                            Nothing ->
                                                ""

                                            Just justAttributes ->
                                                case Dict.get "for" justAttributes of
                                                    Nothing ->
                                                        ""

                                                    Just justFor ->
                                                        justFor

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
                                tree (Flesh { id = 0, for = for, content = content }) []

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
            Just (tree Root elements)

        Err _ ->
            Nothing


toHtml : Tree Element -> Html msg
toHtml tree =
    text ""
