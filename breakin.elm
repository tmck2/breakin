module Main exposing (..)

import Html exposing (Html, button, div, text, hr)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard
import AnimationFrame
import Time exposing (Time)
import Array


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { lives : Int
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
color i =
    let
        colors =
            Array.fromList [ "rgb(92, 199, 42)", "rgb(35, 139, 214)", "rgb(205, 112, 2)" ]
    in
        case Array.get (i % (Array.length colors)) colors of
            Just c ->
                c

            Nothing ->
                "rgb(196, 109, 204)"


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
    , sx = toFloat 40
    , sy = toFloat 20
    , vx = toFloat 0
    , vy = toFloat 0
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
        bricks
            |> List.filter (\brick -> brick.id /= Brick id)


init : ( Model, Cmd Msg )
init =
    ( { paddle =
            { id = Paddle
            , x = 160
            , y = 400
            , sx = 120
            , sy = 20
            , vx = 0
            , vy = 0
            , color = "#ddd"
            }
      , bricks =
            [2..7]
                |> List.concatMap (\i -> createRow i i 10)
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
      , state = Serving
      , lives = 3
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | Update Time


checkBounds entity =
    case entity.id of
        Paddle ->
            { entity | x = min (max entity.x 0) 330 }

        Ball ->
            if entity.y <= 0 then
                { entity | y = 0, vy = -entity.vy }
            else if entity.x >= 430 then
                { entity | x = 430, vx = -entity.vx }
            else if entity.x <= 0 then
                { entity | x = 0, vx = -entity.vx }
            else
                entity

        _ ->
            entity


ptInRectangle ( minx, miny, maxx, maxy ) ( x, y ) =
    x >= minx && x <= maxx && y >= miny && y <= maxy


boundingRect entity =
    ( entity.x, entity.y, entity.x + entity.sx, entity.y + entity.sy )


entityVerts e =
    [ ( e.x, e.y )
    , ( e.x + e.sx, e.y )
    , ( e.x + e.sy, e.y + e.sy )
    , ( e.x, e.y + e.sy )
    ]


colliding : Entity -> Entity -> Bool
colliding e1 e2 =
    List.any (ptInRectangle (boundingRect e2)) (entityVerts e1)
        || List.any (ptInRectangle (boundingRect e1)) (entityVerts e2)


handleCollisions : Model -> Model
handleCollisions model =
    let
        ball =
            model.ball

        paddle =
            model.paddle

        bricks =
            model.bricks

        brickCollisions =
            bricks
                |> List.filter (colliding ball)

        ( cx, cy ) =
            ( ball.x + 10, ball.y + 10 )
    in
        if not <| List.isEmpty brickCollisions then
            case List.head brickCollisions of
                Just brick ->
                    { model
                        | bricks = removeBrick brick bricks
                        , ball =
                            { ball
                                | vx =
                                    if cx <= brick.x || cx >= (brick.x + brick.sx) then
                                        -ball.vx
                                    else
                                        ball.vx
                                , vy =
                                    if cy <= brick.y || cy >= (brick.y + brick.sy) then
                                        -ball.vy
                                    else
                                        ball.vy
                            }
                    }

                Nothing ->
                    model
        else if colliding model.ball model.paddle then
            { model
                | ball = { ball | vx = ball.vx, vy = -ball.vy, y = paddle.y - ball.sy }
            }
        else
            model


updatePaddle : Msg -> Model -> Model
updatePaddle msg model =
    let
        paddle =
            model.paddle
    in
        case msg of
            KeyDown keyCode ->
                if keyCode == 37 then
                    { model | paddle = { paddle | vx = -3 } }
                else if keyCode == 39 then
                    { model | paddle = { paddle | vx = 3 } }
                else
                    model

            KeyUp keyCode ->
                { model | paddle = { paddle | vx = 0 } }

            Update time ->
                { model | paddle = checkBounds { paddle | x = paddle.x + paddle.vx } }


updateBall : Msg -> Model -> Model
updateBall msg model =
    let
        ball =
            model.ball

        paddle =
            model.paddle
    in
        case msg of
            KeyUp keyCode ->
                if keyCode == 17 && model.state == Serving then
                    { model | state = InPlay, ball = { ball | vx = toFloat 3, vy = toFloat -3 } }
                else
                    { model | paddle = { paddle | vx = toFloat 0 } }

            Update time ->
                case model.state of
                    Serving ->
                        { model
                            | ball =
                                { ball
                                    | x = paddle.x + paddle.sx / 2 - ball.sx / 2
                                    , y = paddle.y - ball.sy
                                }
                        }

                    InPlay ->
                        { model
                            | ball =
                                model.ball
                                    |> (\entity ->
                                            { entity
                                                | x = entity.x + entity.vx
                                                , y = entity.y + entity.vy
                                            }
                                       )
                                    |> checkBounds
                        }

            _ ->
                model


updateAlive : Model -> Model
updateAlive model =
    let
        ball =
            model.ball

        state =
            model.state
    in
        { model
            | state =
                if ball.y > 450 then
                    Serving
                else
                    state
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
        |> (updatePaddle msg)
        |> (updateBall msg)
        |> handleCollisions
        |> updateAlive
    , Cmd.none
    )


subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Update
        ]



-- VIEW


px : Float -> String
px val =
    toString (round val) ++ "px"


renderEntity : Entity -> Html a
renderEntity entity =
    div
        [ style
            [ "position" => "absolute"
            , "left" => px entity.x
            , "top" => px entity.y
            , "width" => px entity.sx
            , "height" => px entity.sy
            , "background-color" => entity.color
            , "display" => "inline-block"
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div
        [ style
            [ "background-color" => "#eee"
            , "width" => "100%"
            , "height" => "100%"
            ]
        ]
        [ div
            [ style
                [ "position" => "relative"
                , "width" => "455px"
                , "height" => "500px"
                , "margin" => "auto"
                , "background-color" => "rgb(47, 36, 55)"
                ]
            ]
            [ div [] (List.map renderEntity model.bricks)
            , div
                [ style
                    [ "position" => "absolute"
                    , "top" => "500px"
                    ]
                ]
                []
              --[ hr [] []
              --, text <| toString model
              --]
            , renderEntity model.paddle
            , renderEntity model.ball
            ]
        ]
