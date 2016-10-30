module Model exposing (..)

import Array
import Time exposing (Time)
import Keyboard
import Json.Decode.Pipeline exposing (required, decode)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import String exposing (toInt, words, startsWith)


sounds =
    { break = "bottle_pop_3.wav"
    , die = "record-scratch-1.wav"
    }


type Msg
    = KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | Update Time
    | Pause
    | UpdateModel String
    | Rewind
    | FastForward


type alias Model =
    { state : State
    , counter : Int
    , lives : Int
    , score : Int
    , paddle : Paddle
    , ball : Entity
    , bricks : List Entity
    }


type State
    = Title
    | Paused
    | Serving
    | InPlay


type EntityId
    = Brick Int
    | Paddle
    | Ball


type alias Entity =
    { id : EntityId
    , x : Float
    , y : Float
    , sx : Float
    , sy : Float
    , vx : Float
    , vy : Float
    , color : String
    }


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


encodeModel : Model -> Encode.Value
encodeModel { counter, lives, score, paddle, bricks, ball, state } =
    object
        [ ( "state", Encode.string (toString state) )
        , ( "counter", Encode.int counter )
        , ( "lives", Encode.int lives )
        , ( "score", Encode.int score )
        , ( "paddle", encodeEntity paddle )
        , ( "ball", encodeEntity ball )
        , ( "bricks", Encode.list (bricks |> List.map encodeEntity) )
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


type alias Paddle =
    Entity


color : Int -> String
color ix =
    Array.fromList [ "rgb(92, 199, 42)", "rgb(35, 139, 214)", "rgb(205, 112, 2)" ]
        |> Array.get (ix % 3)
        |> Maybe.withDefault "red"


createRow : Int -> Int -> Int -> List Entity
createRow seed rowNum len =
    if len == 0 then
        []
    else
        (createBrick rowNum (len - 1) (color seed)) :: (createRow (rowNum * seed + len) rowNum (len - 1))


createBrick : Int -> Int -> String -> Entity
createBrick rowNum colNum color =
    { id = Brick (rowNum * 10 + colNum)
    , x = toFloat <| 5 + 5 * colNum + colNum * 40
    , y = toFloat <| 20 * rowNum + 5 + 5 * rowNum
    , sx = 40.0
    , sy = 20.0
    , vx = 0.0
    , vy = 0.0
    , color = color
    }


removeBrick : Entity -> List Entity -> List Entity
removeBrick entity bricks =
    let
        id =
            case entity.id of
                Brick id ->
                    id

                _ ->
                    -1
    in
        bricks |> List.filter (\brick -> brick.id /= Brick id)
