module Update exposing (..)

import Model exposing (..)
import AnimationFrame
import Keyboard
import Array
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import Char exposing (toCode)
import Serialization exposing (..)
import Levels exposing (..)
import Ports exposing (..)


brickCollisions : Model -> ( Model, List (Cmd Msg) )
brickCollisions model =
    let
        { ball, paddle, bricks } =
            model
    in
        case List.head <| List.filter (colliding ball) bricks of
            Just brick ->
                let
                    ys =
                        [ ball.y, ball.y + ball.sy ]

                    ( vx, vy ) =
                        if
                            List.all ((<=) (brick.y + brick.sy - 4)) ys
                                || List.all ((>=) (brick.y + 4)) ys
                        then
                            ( ball.vx, -ball.vy )
                        else
                            ( -ball.vx, ball.vy )
                in
                    ( { model
                        | bricks = removeBrick brick bricks
                        , ball =
                            { ball
                                | vx = vx
                                , vy = vy
                            }
                        , score = model.score + 1
                      }
                    , [ playSound sounds.break ]
                    )

            Nothing ->
                ( model, [ Cmd.none ] )


paddleCollisions : Model -> ( Model, List (Cmd Msg) )
paddleCollisions model =
    let
        { ball, paddle } =
            model

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
updatePaddle msg model =
    let
        paddle =
            model.paddle

        ( leftKey, rightKey ) =
            ( 37, 39 )
    in
        case msg of
            KeyDown keyCode ->
                if keyCode == leftKey then
                    ( { model | paddle = { paddle | vx = -3.5 } }, [ Cmd.none ] )
                else if keyCode == rightKey then
                    ( { model | paddle = { paddle | vx = 3.5 } }, [ Cmd.none ] )
                else
                    ( model, [ Cmd.none ] )

            KeyUp keyCode ->
                ( { model | paddle = { paddle | vx = 0 } }, [ Cmd.none ] )

            Update time ->
                ( { model | paddle = (checkBounds 455) { paddle | x = paddle.x + paddle.vx } }, [ Cmd.none ] )

            _ ->
                ( model, [ Cmd.none ] )


updateBallInPlay : Msg -> Model -> ( Model, List (Cmd Msg) )
updateBallInPlay msg model =
    let
        { ball, paddle } =
            model
    in
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
                            |> (checkBounds 455)
                  }
                , [ Cmd.none ]
                )

            _ ->
                ( model, [ Cmd.none ] )


updateBallServing : Msg -> Model -> ( Model, List (Cmd Msg) )
updateBallServing msg model =
    let
        { ball, paddle } =
            model

        ctrlKey =
            17
    in
        case msg of
            KeyUp keyCode ->
                if keyCode == ctrlKey then
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
updateAlive model =
    let
        lives =
            model.lives - 1
    in
        if model.ball.y > 450 then
            ( { model
                | state =
                    if lives > 0 then
                        Serving
                    else
                        GameOver
                , lives = lives
              }
            , List.concat
                [ if lives > 0 then
                    [ playSound sounds.die ]
                  else
                    [ playSound sounds.die, saveHighScore model.score, getHighScore () ]
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
updateLevel model =
    if List.length model.bricks <= 0 then
        let
            { paddle } =
                model

            level =
                model.level + 1

            bricksForLevel n =
                case Array.get n levels of
                    Just bricks ->
                        bricksFromCharMap bricks

                    Nothing ->
                        []

            paddleWidth =
                max 40 (paddle.sx * 0.8)
        in
            ( { model | state = Serving, level = level, bricks = bricksForLevel (level % 3), paddle = { paddle | sx = paddleWidth } }, [ Cmd.none ] )
    else
        ( model, [ Cmd.none ] )


checkForPause : Msg -> Model -> ( Model, List (Cmd Msg) )
checkForPause msg model =
    case msg of
        KeyUp keycode ->
            if keycode == toCode 'P' then
                ( { model
                    | state =
                        if model.state == Paused then
                            InPlay
                        else
                            Paused
                  }
                , [ Cmd.none ]
                )
            else
                ( model, [ Cmd.none ] )

        _ ->
            ( model, [ Cmd.none ] )


paused : Msg -> Model -> ( Model, Cmd Msg )
paused msg model =
    case msg of
        KeyUp keycode ->
            if keycode == toCode 'P' then
                ( { model | state = InPlay }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyDown keycode ->
            if keycode == toCode 'R' then
                ( model, rewind () )
            else if keycode == toCode 'F' then
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
                        ( { model | state = Paused }, Cmd.none )

                    Err msg ->
                        Debug.crash (Debug.log "" msg)

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
        KeyUp _ ->
            ( { model | state = Serving }, Cmd.none )

        _ ->
            ( model, Cmd.none )


gameOver : Msg -> Model -> ( Model, Cmd Msg )
gameOver msg model =
    let
        { paddle } =
            model
    in
        case msg of
            KeyPress _ ->
                ( initialModel, Cmd.none )

            UpdateHighScore score ->
                ( { model | highScore = score }, Cmd.none )

            _ ->
                ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.state of
        Title ->
            title msg model

        Serving ->
            serving msg model

        InPlay ->
            inPlay msg model

        Paused ->
            paused msg model

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
