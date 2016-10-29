port module Update exposing (..)

import Model exposing (..)
import AnimationFrame
import Keyboard


port playSound : String -> Cmd msg


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
        { ball, paddle, bricks } =
            model

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
                    , [ playSound sounds.break ]
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
                , [ playSound sounds.break ]
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
                    ( { model | paddle = { paddle | vx = -4 } }, [ Cmd.none ] )
                else if keyCode == 39 then
                    ( { model | paddle = { paddle | vx = 4 } }, [ Cmd.none ] )
                else
                    ( model, [ Cmd.none ] )

            KeyUp keyCode ->
                ( { model | paddle = { paddle | vx = 0 } }, [ Cmd.none ] )

            Update time ->
                ( { model | paddle = checkBounds { paddle | x = paddle.x + paddle.vx } }, [ Cmd.none ] )

            _ ->
                ( model, [ Cmd.none ] )


updateBallInPlay : Msg -> Model -> ( Model, List (Cmd Msg) )
updateBallInPlay msg model =
    let
        { ball, paddle } =
            model
    in
        case msg of
            KeyUp keyCode ->
                ( { model | paddle = { paddle | vx = toFloat 0 } }, [ Cmd.none ] )

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
                            |> checkBounds
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
    in
        case msg of
            KeyUp keyCode ->
                if keyCode == 17 then
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
    if model.ball.y > 450 && model.state == InPlay then
        ( { model | state = Serving, lives = model.lives - 1 }, [ playSound sounds.die ] )
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


paused : Msg -> Model -> ( Model, Cmd Msg )
paused msg model =
    ( model, Cmd.none )


serving : Msg -> Model -> ( Model, Cmd Msg )
serving msg model =
    let
        ( updatedModel, cmds ) =
            model
                |> updateIncrementCounter
                >>= updatePaddle msg
                >>= updateBallServing msg
                >>= handleCollisions
                >>= updateAlive
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
                >>= handleCollisions
                >>= updateAlive
    in
        ( updatedModel, Cmd.batch cmds )


title : Msg -> Model -> ( Model, Cmd Msg )
title msg model =
    case msg of
        KeyUp _ ->
            ( { model | state = Serving }, Cmd.none )

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


subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Update
        ]
