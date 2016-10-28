module Model exposing (..)

import Array
import Time exposing (Time)
import Keyboard


sounds =
    { break = "bottle_pop_3.wav"
    , die = "record-scratch-1.wav"
    }


type Msg
    = KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | Update Time
    | PlaySound String


type alias Model =
    { lives : Int
    , score : Int
    , paddle : Paddle
    , bricks : List Entity
    , ball : Entity
    , state : State
    }


type State
    = Serving
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
