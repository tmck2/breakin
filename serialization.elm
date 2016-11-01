module Serialization exposing (..)

import Model exposing (..)
import Json.Decode.Pipeline exposing (required, decode)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import String exposing (toInt, words, startsWith)


encodeModel : Model -> Encode.Value
encodeModel { counter, lives, score, paddle, bricks, ball, state, level } =
    object
        [ ( "state", Encode.string (toString state) )
        , ( "counter", Encode.int counter )
        , ( "lives", Encode.int lives )
        , ( "score", Encode.int score )
        , ( "paddle", encodeEntity paddle )
        , ( "ball", encodeEntity ball )
        , ( "bricks", Encode.list (bricks |> List.map encodeEntity) )
        , ( "level", Encode.int level )
        ]


decodeModel : Decode.Decoder Model
decodeModel =
    decode Model
        |> required "state" decodeState
        |> required "counter" Decode.int
        |> required "lives" Decode.int
        |> required "score" Decode.int
        |> required "paddle" decodeEntity
        |> required "ball" decodeEntity
        |> required "bricks" (Decode.list decodeEntity)
        |> required "level" Decode.int


encodeEntity : Entity -> Encode.Value
encodeEntity { id, x, y, sx, sy, vx, vy, color } =
    object
        [ ( "id", Encode.string <| toString id )
        , ( "x", Encode.float x )
        , ( "y", Encode.float y )
        , ( "sx", Encode.float sx )
        , ( "sy", Encode.float sy )
        , ( "vx", Encode.float vx )
        , ( "vy", Encode.float vy )
        , ( "color", Encode.string color )
        ]


decodeEntity : Decode.Decoder Entity
decodeEntity =
    decode Entity
        |> required "id" decodeEntityId
        |> required "x" Decode.float
        |> required "y" Decode.float
        |> required "sx" Decode.float
        |> required "sy" Decode.float
        |> required "vx" Decode.float
        |> required "vy" Decode.float
        |> required "color" Decode.string


decodeState : Decoder State
decodeState =
    customDecoder value
        (\val ->
            case decodeValue Decode.string val of
                Ok state ->
                    case state of
                        "Title" ->
                            Ok Title

                        "Paused" ->
                            Ok Paused

                        "Serving" ->
                            Ok Serving

                        "InPlay" ->
                            Ok InPlay

                        _ ->
                            Result.Err "invalid value in state"

                _ ->
                    Result.Err "invalid value in state"
        )


decodeEntityId : Decoder EntityId
decodeEntityId =
    customDecoder value
        (\val ->
            case decodeValue Decode.string val of
                Ok id ->
                    if startsWith "Brick" id then
                        case words id of
                            _ :: numStr :: _ ->
                                case toInt numStr of
                                    Ok num ->
                                        Ok (Brick num)

                                    _ ->
                                        Result.Err "unexpected value specified for brick id"

                            _ ->
                                Result.Err "unexpected value specified for brick id"
                    else if id == "Paddle" then
                        Ok Paddle
                    else if id == "Ball" then
                        Ok Ball
                    else
                        Result.Err "unexpected value specified for entity id"

                _ ->
                    Result.Err "unexpected value specified for entity"
        )
