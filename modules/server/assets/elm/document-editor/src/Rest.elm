module Rest exposing (escapeString, keyDecoder, mouseDecoder, shiftKeyDecoder)

import Json.Decode as Decode
import Json.Encode as Encode


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


shiftKeyDecoder : Decode.Decoder Bool
shiftKeyDecoder =
    Decode.field "shiftKey" Decode.bool


mouseDecoder : Decode.Decoder (Maybe String)
mouseDecoder =
    Decode.field "relatedTarget" (Decode.maybe Decode.string)


escapeString : String -> String
escapeString string =
    let
        encodedString =
            Encode.encode 0 (Encode.string string)

        encodedStringLength =
            String.length encodedString
    in
    String.slice 1 (encodedStringLength - 1) encodedString
