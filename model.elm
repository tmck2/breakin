module Model exposing (..)

import String
import Array
import Time exposing (Time)
import Keyboard
import Levels exposing (..)


initialModel =
    { counter = 0
    , paddle =
        { id = Paddle
        , x = 160
        , y = 400
        , sx = 100
        , sy = 20
        , vx = 0
        , vy = 0
        , color = "#ddd"
        }
    , bricks =
        bricksFromCharMap level1
    , ball =
        { id = Ball
        , x = 210
        , y = 380
        , sx = 20
        , sy = 20
        , vx = 0
        , vy = 0
        , color = "#bbb"
        }
    , state = Title
    , lives = 3
    , score = 0
    , level = 0
    }


mapCharToColor ch =
    case ch of
        'Y' ->
            "yellow"

        'R' ->
            "rgb(230, 52, 116)"

        'W' ->
            "white"

        'G' ->
            "rgb(17, 167, 72)"

        _ ->
            "gray"


bricksFromCharMap charMap =
    (charMap
        |> List.map2 (,) [0..11]
        |> List.map (\( row, str ) -> ( row, List.indexedMap (,) (String.toList str) ))
        |> List.concatMap (\( row, cols ) -> cols |> List.map (\( col, ch ) -> ( row, col, ch )))
        |> List.filter (\( row, col, ch ) -> ch /= '.')
        |> List.map (\( row, col, ch ) -> brick row col (mapCharToColor ch))
    )


brick row col color =
    { id = Brick (row * 10 + col)
    , x = toFloat <| 5 + 5 * col + col * 40
    , y = toFloat <| 20 * row + 5 + 5 * row
    , sx = 40.0
    , sy = 20.0
    , vx = 0.0
    , vy = 0.0
    , color = color
    }


brickFromChar char row col =
    case char of
        "X" ->
            Just <| brick row col "yellow"

        _ ->
            Nothing


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
    , level : Int
    }


type State
    = Title
    | Paused
    | Serving
    | InPlay
    | GameOver


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
