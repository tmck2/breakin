module Model exposing (..)

import String
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
    , highScore = 0
    }


sounds =
    { break = "bottle_pop_3.wav"
    , die = "record-scratch-1.wav"
    }


type Msg
    = KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | KeyPress Keyboard.KeyCode
    | Update Time
    | Pause
    | UpdateModel String
    | Rewind
    | FastForward
    | UpdateHighScore Int


type alias Model =
    { state : State
    , counter : Int
    , lives : Int
    , score : Int
    , paddle : Paddle
    , ball : Entity
    , bricks : List Entity
    , level : Int
    , highScore : Int
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


colliding : Entity -> Entity -> Bool
colliding e1 e2 =
    let
        ptInRectangle ( minx, miny, maxx, maxy ) ( x, y ) =
            x > minx && x < maxx && y > miny && y < maxy

        boundingRect entity =
            ( entity.x, entity.y, entity.x + entity.sx, entity.y + entity.sy )

        entityVerts e =
            [ ( e.x, e.y )
            , ( e.x + e.sx, e.y )
            , ( e.x + e.sy, e.y + e.sy )
            , ( e.x, e.y + e.sy )
            ]
    in
        List.any (ptInRectangle (boundingRect e2)) (entityVerts e1)
            || List.any (ptInRectangle (boundingRect e1)) (entityVerts e2)


checkBounds : Float -> Entity -> Entity
checkBounds width entity =
    case entity.id of
        Paddle ->
            { entity | x = min (max entity.x 0) (width - entity.sx) }

        Ball ->
            if entity.y <= 0 then
                { entity | y = 0, vy = -entity.vy }
            else if entity.x >= (width - entity.sx) then
                { entity | x = (width - entity.sx), vx = -entity.vx }
            else if entity.x <= 0 then
                { entity | x = 0, vx = -entity.vx }
            else
                entity

        _ ->
            entity
