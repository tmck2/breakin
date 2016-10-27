port module Main exposing (..)

import Html exposing (Html, button, div, text, hr)
import Html.App as App
import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)
import Keyboard
import AnimationFrame
import Time exposing (Time)
import Array
import Task exposing (perform, succeed, fail)


port playSound : String -> Cmd msg


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


init : ( Model, Cmd Msg )
init =
    ( { paddle =
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
            [2..7] |> List.concatMap (\i -> createRow i i 10)
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
      , score = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode
    | Update Time
    | PlaySound String


checkBounds entity =
    case entity.id of
        Paddle ->
            { entity | x = min (max entity.x 0) (455 - entity.sx) }

        Ball ->
            if entity.y <= 0 then
                { entity | y = 0, vy = -entity.vy }
            else if entity.x >= (455 - entity.sx) then
                { entity | x = (455 - entity.sx), vx = -entity.vx }
            else if entity.x <= 0 then
                { entity | x = 0, vx = -entity.vx }
            else
                entity

        _ ->
            entity


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


colliding : Entity -> Entity -> Bool
colliding e1 e2 =
    List.any (ptInRectangle (boundingRect e2)) (entityVerts e1)
        || List.any (ptInRectangle (boundingRect e1)) (entityVerts e2)


handleCollisions : Model -> ( Model, List (Cmd Msg) )
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
            ( ball.x + ball.sx / 2, ball.y + ball.sy / 2 )
    in
        if not <| List.isEmpty brickCollisions then
            case List.head brickCollisions of
                Just brick ->
                    ( { model
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
                        , score = model.score + 1
                      }
                    , [ playSound "bottle_pop_3.wav" ]
                    )

                Nothing ->
                    ( model, [ Cmd.none ] )
        else if colliding model.ball model.paddle then
            let
                normalize ( x, y ) =
                    let
                        mag =
                            sqrt (x * x + y * y)
                    in
                        ( x / mag, y / mag )

                d =
                    sin ((max ((ball.x + ball.sx / 2 - paddle.x) / paddle.sx) 0.0) * pi)

                sign x =
                    if x >= 0.0 then
                        1.0
                    else
                        -1.0

                ( vx, vy ) =
                    normalize ( d * 1 + (1 - d) * 2, d * 2 + (1 - d) * 1 )
            in
                ( { model
                    | ball = { ball | vx = 4 * (sign ball.vx) * vx, vy = -4 * (sign ball.vy) * vy, y = paddle.y - ball.sy }
                  }
                , [ playSound "bottle_pop_3.wav" ]
                )
        else
            ( model, [ Cmd.none ] )


updatePaddle : Msg -> Model -> ( Model, List (Cmd Msg) )
updatePaddle msg model =
    let
        paddle =
            model.paddle
    in
        case msg of
            KeyDown keyCode ->
                if keyCode == 37 then
                    ( { model | paddle = { paddle | vx = -3 } }, [ Cmd.none ] )
                else if keyCode == 39 then
                    ( { model | paddle = { paddle | vx = 3 } }, [ Cmd.none ] )
                else
                    ( model, [ Cmd.none ] )

            KeyUp keyCode ->
                ( { model | paddle = { paddle | vx = 0 } }, [ Cmd.none ] )

            Update time ->
                ( { model | paddle = checkBounds { paddle | x = paddle.x + paddle.vx } }, [ Cmd.none ] )

            _ ->
                ( model, [ Cmd.none ] )


updateBall : Msg -> Model -> ( Model, List (Cmd Msg) )
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
                    ( { model | state = InPlay, ball = { ball | vx = toFloat 3, vy = toFloat -3 } }, [ Cmd.none ] )
                else
                    ( { model | paddle = { paddle | vx = toFloat 0 } }, [ Cmd.none ] )

            Update time ->
                case model.state of
                    Serving ->
                        ( { model
                            | ball =
                                { ball
                                    | x = paddle.x + paddle.sx / 2 - ball.sx / 2
                                    , y = paddle.y - ball.sy
                                }
                          }
                        , [ Cmd.none ]
                        )

                    InPlay ->
                        ( { model
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
                        , [ Cmd.none ]
                        )

            _ ->
                ( model, [ Cmd.none ] )


updateAlive : Model -> ( Model, List (Cmd Msg) )
updateAlive model =
    if model.ball.y > 450 && model.state == InPlay then
        ( { model | state = Serving, lives = model.lives - 1 }, [ playSound "record-scratch-1.wav" ] )
    else
        ( model, [ Cmd.none ] )


(>>=) update1 update2 model =
    let
        ( m1, c1 ) =
            update1 model

        ( m2, c2 ) =
            update2 m1
    in
        ( m2, List.append c1 c2 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( updatedModel, cmds ) =
            model
                |> (updatePaddle msg)
                >>= (updateBall msg)
                >>= handleCollisions
                >>= updateAlive
    in
        ( updatedModel, Cmd.batch cmds )


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
        [ class "entity"
        , style
            [ "left" => px entity.x
            , "top" => px entity.y
            , "width" => px entity.sx
            , "height" => px entity.sy
            , "background-color" => entity.color
            ]
        ]
        []


renderScore : Int -> Html a
renderScore score =
    div [ id "score" ] [ text (toString score) ]


renderLives : Int -> Html a
renderLives num =
    div
        [ id "lives-container" ]
        [ div [ style [] ]
            (List.map (\n -> div [ id "life" ] []) [1..num])
        ]


view : Model -> Html Msg
view model =
    div
        [ id "playing-field" ]
        [ renderScore model.score
        , div [] (List.map renderEntity model.bricks)
        , renderEntity model.paddle
        , renderEntity model.ball
        , renderLives model.lives
        ]
