module Update exposing (..)

import Model exposing (..)
import AnimationFrame
import Keyboard
import Mouse
import Array
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import Char
import Serialization exposing (..)
import Levels exposing (..)
import Ports exposing (..)
import TouchEvents


keys =
    { left = 37
    , right = 39
    , control = 17
    , p = Char.toCode 'P'
    , r = Char.toCode 'R'
    , f = Char.toCode 'F'
    }


center : Entity -> ( Float, Float )
center ({ x, y, sx, sy } as entity) =
    (( x + sx / 2, y + sy / 2 ))


distance : Entity -> Entity -> Float
distance entity1 entity2 =
    let
        ( p1, p2 ) =
            ( center entity1, center entity2 )

        ( x, y ) =
            ( fst p2 - fst p1, snd p2 - snd p1 )
    in
        sqrt x * x + y * y


brickCollisions : Model -> ( Model, List (Cmd Msg) )
brickCollisions ({ ball, paddle, bricks } as model) =
    case List.head <| List.sortBy (distance ball) <| List.filter (colliding ball) bricks of
        Just brick ->
            let
                ys =
                    [ ball.y, ball.y + ball.sy ]

                ( vx, vy ) =
                    if List.all ((<=) (brick.y + brick.sy - 4)) ys && ball.vy < 0 then
                        ( ball.vx, -ball.vy )
                    else if List.all ((>=) (brick.y + 4)) ys && ball.vy > 0 then
                        ( ball.vx, -ball.vy )
                    else
                        ( -ball.vx, ball.vy )
            in
                ( { model
                    | bricks = removeBrick brick bricks
                    , ball =
                        { ball
                            | x = ball.x - ball.vx * 1.5
                            , y = ball.y - ball.vy * 1.5
                            , vx = vx
                            , vy = vy
                        }
                    , score = model.score + 1
                  }
                , [ playSound ( 0.8, False, sounds.break ) ]
                )

        Nothing ->
            ( model, [ Cmd.none ] )


paddleCollisions : Model -> ( Model, List (Cmd Msg) )
paddleCollisions ({ ball, paddle } as model) =
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
        if colliding paddle ball && ball.y < paddle.y + 2 then
            ( { model
                | ball = { ball | vx = 4 * (sign ball.vx) * vx, vy = -4 * (sign ball.vy) * vy, y = paddle.y - ball.sy }
              }
            , [ Cmd.none ]
            )
        else
            ( model, [ Cmd.none ] )


updatePaddle : Msg -> Model -> ( Model, List (Cmd Msg) )
updatePaddle msg ({ paddle } as model) =
    case msg of
        MouseDown ({ clientX, clientY } as touch) ->
            if clientX < model.screenWidth / 2 then
                ( { model | paddle = { paddle | vx = -3.5 } }, [ Cmd.none ] )
            else
                ( { model | paddle = { paddle | vx = 3.5 } }, [ Cmd.none ] )

        MouseUp _ ->
            ( { model | paddle = { paddle | vx = 0 } }, [ Cmd.none ] )

        KeyDown keyCode ->
            if keyCode == keys.left then
                ( { model | paddle = { paddle | vx = -3.5 } }, [ Cmd.none ] )
            else if keyCode == keys.right then
                ( { model | paddle = { paddle | vx = 3.5 } }, [ Cmd.none ] )
            else
                ( model, [ Cmd.none ] )

        KeyUp keyCode ->
            ( { model | paddle = { paddle | vx = 0 } }, [ Cmd.none ] )

        Update time ->
            ( { model | paddle = (checkBounds ( model.screenWidth, model.screenHeight )) { paddle | x = paddle.x + paddle.vx } }, [ Cmd.none ] )

        _ ->
            ( model, [ Cmd.none ] )


updateBallInPlay : Msg -> Model -> ( Model, List (Cmd Msg) )
updateBallInPlay msg ({ ball, paddle } as model) =
    case msg of
        Update time ->
            ( { model
                | ball =
                    model.ball
                        |> (\entity ->
                                { entity
                                    | x = entity.x + entity.vx
                                    , y = entity.y + entity.vy
                                }
                           )
                        |> (checkBounds ( model.screenWidth, model.screenHeight ))
              }
            , [ Cmd.none ]
            )

        _ ->
            ( model, [ Cmd.none ] )


updateBallServing : Msg -> Model -> ( Model, List (Cmd Msg) )
updateBallServing msg ({ ball, paddle } as model) =
    case msg of
        MouseUp { clientX, clientY } ->
            if
                ((clientY < (model.screenHeight - model.screenHeight / 4))
                    && (clientX > 100)
                    && (clientX < model.screenWidth - 100)
                )
            then
                ( { model | state = InPlay, ball = { ball | vx = toFloat 3, vy = toFloat -3 } }, [ Cmd.none ] )
            else
                ( model, [ Cmd.none ] )

        KeyUp keyCode ->
            if keyCode == keys.control then
                ( { model | state = InPlay, ball = { ball | vx = toFloat 3, vy = toFloat -3 } }, [ Cmd.none ] )
            else
                ( model, [ Cmd.none ] )

        Update time ->
            ( { model
                | ball =
                    { ball
                        | x = paddle.x + paddle.sx / 2 - ball.sx / 2
                        , y = paddle.y - ball.sy
                    }
              }
            , [ Cmd.none ]
            )

        _ ->
            ( model, [ Cmd.none ] )


updateAlive : Model -> ( Model, List (Cmd Msg) )
updateAlive ({ lives } as model) =
    if model.ball.y > model.screenHeight then
        ( { model
            | state =
                if lives > 0 then
                    Serving
                else
                    GameOver
            , lives = lives - 1
          }
        , List.concat
            [ if lives > 0 then
                [ playSound ( 0.5, False, sounds.die ) ]
              else
                [ playSound ( 0.5, False, sounds.die ), saveHighScore model.score, getHighScore () ]
            ]
        )
    else
        ( model, [ Cmd.none ] )


updateIncrementCounter : Model -> ( Model, List (Cmd Msg) )
updateIncrementCounter model =
    ( { model | counter = model.counter + 1 }, [ Cmd.none ] )


(>>=) update1 update2 model =
    let
        ( m1, c1 ) =
            update1 model

        ( m2, c2 ) =
            update2 m1
    in
        ( m2, List.append c1 c2 )


updateLevel : Model -> ( Model, List (Cmd Msg) )
updateLevel ({ paddle, level } as model) =
    if List.length model.bricks <= 0 then
        ( { model
            | state = Serving
            , level = level + 1
            , bricks =
                bricksFromCharMap ( model.screenWidth, model.screenHeight ) <|
                    Maybe.withDefault [] <|
                        Array.get ((level + 1) % Array.length levels) levels
            , paddle = { paddle | sx = max 40 (paddle.sx * 0.8) }
          }
        , [ Cmd.none ]
        )
    else
        ( model, [ Cmd.none ] )


checkForPause : Msg -> Model -> ( Model, List (Cmd Msg) )
checkForPause msg model =
    case msg of
        KeyUp keycode ->
            if keycode == keys.p then
                ( { model | paused = not model.paused }, [ Cmd.none ] )
            else
                ( model, [ Cmd.none ] )

        _ ->
            ( model, [ Cmd.none ] )


paused : Msg -> Model -> ( Model, Cmd Msg )
paused msg model =
    case msg of
        KeyUp keycode ->
            if keycode == keys.p then
                ( { model | paused = False }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyDown keycode ->
            if keycode == keys.r then
                ( model, rewind () )
            else if keycode == keys.f then
                ( model, fastforward () )
            else
                ( model, Cmd.none )

        UpdateModel modelJson ->
            let
                modelResult =
                    decodeString decodeModel modelJson
            in
                case modelResult of
                    Ok model ->
                        ( { model | paused = True }, Cmd.none )

                    Err msg ->
                        Debug.crash msg

        _ ->
            ( model, Cmd.none )


updateSaveState : Model -> ( Model, List (Cmd Msg) )
updateSaveState model =
    ( model, [ saveState (encode 0 (encodeModel model)) ] )


serving : Msg -> Model -> ( Model, Cmd Msg )
serving msg model =
    let
        ( updatedModel, cmds ) =
            model
                |> updateIncrementCounter
                >>= updatePaddle msg
                >>= updateBallServing msg
                >>= checkForPause msg
                >>= updateSaveState
    in
        ( updatedModel, Cmd.batch cmds )


inPlay : Msg -> Model -> ( Model, Cmd Msg )
inPlay msg model =
    let
        ( updatedModel, cmds ) =
            model
                |> updateIncrementCounter
                >>= updatePaddle msg
                >>= updateBallInPlay msg
                >>= brickCollisions
                >>= paddleCollisions
                >>= updateAlive
                >>= updateLevel
                >>= checkForPause msg
                >>= updateSaveState
    in
        ( updatedModel, Cmd.batch cmds )


title : Msg -> Model -> ( Model, Cmd Msg )
title msg model =
    case msg of
        MouseUp _ ->
            ( { model | state = Serving }, playSound ( 0.2, True, sounds.backgroundMusic ) )

        KeyUp _ ->
            ( { model | state = Serving }, playSound ( 0.2, True, sounds.backgroundMusic ) )

        _ ->
            ( model, Cmd.none )


gameOver : Msg -> Model -> ( Model, Cmd Msg )
gameOver msg ({ paddle } as model) =
    case msg of
        MouseUp _ ->
            ( initialModel defaultScreenDimensions, Cmd.none )

        KeyPress _ ->
            ( initialModel defaultScreenDimensions, Cmd.none )

        UpdateHighScore score ->
            ( { model | highScore = score }, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.paused then
        paused msg model
    else
        case model.state of
            Title ->
                title msg model

            Serving ->
                serving msg model

            InPlay ->
                inPlay msg model

            GameOver ->
                gameOver msg model


subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Keyboard.presses KeyPress
        , AnimationFrame.diffs Update
        , updateModel UpdateModel
        , updateHighScore UpdateHighScore
        ]
