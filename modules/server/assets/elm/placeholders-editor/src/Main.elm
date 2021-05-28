module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, hr, i, input, table, tbody, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (attribute, class, disabled, id, scope, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as JsonE
import List.Extra as ListE


type alias Placeholder =
    { key : String, value : String }


type alias Model =
    { placeholders : List Placeholder
    , unsavedChanges : Bool
    }


type Msg
    = Add
    | Remove Int
    | Update Int String String
    | SaveChanges
    | SavePlaceholdersResult (Result Http.Error String)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    List Placeholder


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags False, Cmd.none )


view : Model -> Html Msg
view model =
    let
        placeholders =
            List.indexedMap
                (\index placeholder ->
                    tr []
                        [ th [ scope "row" ]
                            [ text <| String.fromInt <| index + 1 ]
                        , td []
                            [ input
                                [ class "form-control"
                                , type_ "text"
                                , value placeholder.key
                                , onInput <| \newKey -> Update index newKey placeholder.value
                                ]
                                []
                            ]
                        , td []
                            [ textarea
                                [ class "form-control"
                                , id "exampleFormControlTextarea1"
                                , attribute "rows" "2"
                                , value placeholder.value
                                , onInput <| Update index placeholder.key
                                ]
                                []
                            ]
                        , td []
                            [ button
                                [ class "btn btn-outline-danger"
                                , onClick (Remove index)
                                ]
                                [ i [ class "far fa-trash-alt" ] []
                                ]
                            ]
                        ]
                )
                model.placeholders
    in
    div []
        [ h1 [] [ text "List of Placeholders" ]
        , hr [] []
        , button
            [ class "btn btn-outline-success me-2"
            , onClick Add
            ]
            [ i [ class "fas fa-plus me-1" ] [], text "Add" ]
        , button
            [ class "btn btn-outline-success"
            , onClick SaveChanges
            , disabled <| not model.unsavedChanges
            ]
            [ i [ class "fas fa-save me-1" ] [], text "Save Changes" ]
        , hr [] []
        , table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ scope "col" ]
                        [ text "#" ]
                    , th [ scope "col" ]
                        [ text "Key" ]
                    , th [ scope "col" ]
                        [ text "Value" ]
                    , th [ scope "col" ]
                        [ text "Actions" ]
                    ]
                ]
            , tbody []
                placeholders
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, command ) =
            case msg of
                Add ->
                    ( { model
                        | placeholders = List.append model.placeholders <| List.singleton <| Placeholder "lorem" "ipsum"
                        , unsavedChanges = True
                      }
                    , Cmd.none
                    )

                Remove index ->
                    ( { model
                        | placeholders = model.placeholders |> ListE.removeAt index
                        , unsavedChanges = True
                      }
                    , Cmd.none
                    )

                Update index key value_ ->
                    ( { model
                        | placeholders = model.placeholders |> ListE.updateAt index (\_ -> Placeholder key value_)
                        , unsavedChanges = True
                      }
                    , Cmd.none
                    )

                SaveChanges ->
                    let
                        jsonValue =
                            toJsonValue model.placeholders
                    in
                    ( model, Http.post { url = "/placeholders/save", body = Http.jsonBody jsonValue, expect = Http.expectString SavePlaceholdersResult } )

                SavePlaceholdersResult result ->
                    case result of
                        Ok "success" ->
                            ( { model | unsavedChanges = False }, Cmd.none )

                        _ ->
                            ( { model | unsavedChanges = True }, Cmd.none )
    in
    ( newModel, command )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


toJsonValue : List Placeholder -> JsonE.Value
toJsonValue placeholders =
    let
        keyEncoder =
            JsonE.string

        valueEncoder =
            JsonE.string

        placeholderEncoder placeholder =
            JsonE.object [ ( "key", keyEncoder placeholder.key ), ( "value", valueEncoder placeholder.value ) ]

        placeholdersEncoder =
            JsonE.list placeholderEncoder
    in
    placeholdersEncoder placeholders
