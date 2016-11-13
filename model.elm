module Model exposing (..)

import String
import Time exposing (Time)
import Keyboard
import Levels exposing (..)
import Array
import Mouse exposing (Position)
import TouchEvents


brickWidth screenWidth =
    (screenWidth - 55) / 10


brickHeight screenHeight =
    (screenHeight / 25)


ballSize screenY =
    (paddleHeight screenY) * 1.25


paddleWidth screenX =
    (brickWidth screenX) * 2


paddleHeight screenY =
    brickHeight screenY


initialModel : ( Float, Float ) -> Model
initialModel (( screenWidth, screenHeight ) as screenDimensions) =
    { counter = 0
    , paddle =
        { id = Paddle
        , x = (screenWidth - (paddleWidth screenWidth)) / 2
        , y = screenHeight - (paddleHeight screenHeight) * 2
        , sx = paddleWidth screenWidth
        , sy = paddleHeight screenHeight
        , vx = 0
        , vy = 0
        , color = "#ddd"
        }
    , bricks =
        bricksFromCharMap screenDimensions <| Maybe.withDefault level1 (Array.get 0 levels)
    , ball =
        { id = Ball
        , x = 210
        , y = 380
        , sx = ballSize screenHeight
        , sy = ballSize screenHeight
        , vx = 0
        , vy = 0
        , color = "#bbb"
        }
    , state = Title
    , lives = 2
    , score = 0
    , level = 0
    , highScore = 0
    , paused = False
    , screenWidth = screenWidth
    , screenHeight = screenHeight
    }


defaultScreenDimensions =
    ( 667, 375 )


sounds =
    { break = "bottle_pop_3.wav"
    , die = "record-scratch-1.wav"
    , backgroundMusic = "Hard Trance.mp3"
    }


type Msg
    = KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | KeyPress Keyboard.KeyCode
    | Update Time
    | UpdateModel String
    | UpdateHighScore Int
    | MouseDown TouchEvents.Touch
    | MouseUp TouchEvents.Touch


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
    , paused : Bool
    , screenWidth : Float
    , screenHeight : Float
    }


type State
    = Title
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

        'P' ->
            "rgb(230, 52, 116)"

        'W' ->
            "white"

        'G' ->
            "rgb(17, 167, 72)"

        'R' ->
            "rgb(232, 65, 65)"

        _ ->
            "gray"


bricksFromCharMap screenDimensions charMap =
    (charMap
        |> List.map2 (,) [0..11]
        |> List.map (\( row, str ) -> ( row, List.indexedMap (,) (String.toList str) ))
        |> List.concatMap (\( row, cols ) -> cols |> List.map (\( col, ch ) -> ( row, col, ch )))
        |> List.filter (\( row, col, ch ) -> ch /= '.')
        |> List.map (\( row, col, ch ) -> brick screenDimensions row col (mapCharToColor ch))
    )


brick : ( Float, Float ) -> Int -> Int -> String -> Entity
brick (( screenX, screenY ) as screenDimensions) row col color =
    { id = Brick (row * 10 + col)
    , x = toFloat <| 5 + 5 * col + col * (round <| brickWidth screenX)
    , y = toFloat <| (round <| brickHeight screenY) * row + 5 + 5 * row
    , sx = brickWidth screenX
    , sy = brickHeight screenY
    , vx = 0.0
    , vy = 0.0
    , color = color
    }


removeBrick : Entity -> List Entity -> List Entity
removeBrick entity bricks =
    bricks |> List.filter (\e -> e.id /= entity.id)


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


checkBounds : ( Float, Float ) -> Entity -> Entity
checkBounds ( screenX, screenY ) entity =
    case entity.id of
        Paddle ->
            { entity | x = min (max entity.x 0) (screenX - entity.sx) }

        Ball ->
            if entity.y <= 0 then
                { entity | y = 0, vy = -entity.vy }
            else if entity.x >= (screenX - entity.sx) then
                { entity | vx = -entity.vx }
            else if entity.x <= 0 then
                { entity | x = 0, vx = -entity.vx }
            else
                entity

        _ ->
            entity
