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


type EntityId
    = Brick Int
    | Paddle
    | Ball


type alias Entity =
    { id : EntityId
    , x : Int
    , y : Int
    , sx : Int
    , sy : Int
    , vx : Int
    , vy : Int
    , color : String
    }


type alias Paddle =
    Entity


type alias Model =
    { paddle : Paddle
    , bricks : List Entity
    , ball : Entity
    }


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


createRow seed rowNum len =
    if len == 0 then
        []
    else
        (createBrick rowNum (len - 1) (color seed)) :: (createRow (rowNum * seed + len) rowNum (len - 1))


createBrick : Int -> Int -> String -> Entity
createBrick rowNum colNum color =
    { id = Brick (rowNum * 10 + colNum)
    , x = 5 + 5 * colNum + colNum * 40
    , y = 20 * rowNum + 5 + 5 * rowNum
    , sx = 40
    , sy = 20
    , vx = 0
    , vy = 0
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
            , color = "rgb(57, 60, 68)"
            }
      , bricks =
            [0..5]
                |> List.concatMap (\i -> createRow i i 10)
      , ball =
            { id = Ball
            , x = 160
            , y = 380
            , sx = 20
            , sy = 20
            , vx = 3
            , vy = -3
            , color = "rgb(57, 60, 68)"
            }
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


colliding : Entity -> Entity -> Bool
colliding entity1 entity2 =
    entity1.x > entity2.x && entity1.x < (entity2.x + entity2.sx) && entity1.y > entity2.y && entity1.y < (entity2.y + entity2.sy)


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
    in
        if not <| List.isEmpty brickCollisions then
            case List.head brickCollisions of
                Just brick ->
                    { model | bricks = removeBrick brick bricks, ball = { ball | vy = -ball.vy } }

                Nothing ->
                    model
        else if colliding model.ball model.paddle then
            { model | ball = { ball | vy = -ball.vy } }
        else
            model


updatePosition entity =
    { entity | x = entity.x + entity.vx, y = entity.y + entity.vy }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        paddle =
            model.paddle

        ball =
            model.ball
    in
        case msg of
            KeyDown keyCode ->
                if keyCode == 37 then
                    ( { model | paddle = { paddle | vx = -4 } }, Cmd.none )
                else if keyCode == 39 then
                    ( { model | paddle = { paddle | vx = 4 } }, Cmd.none )
                else
                    ( model, Cmd.none )

            KeyUp keyCode ->
                ( { model | paddle = { paddle | vx = 0 } }, Cmd.none )

            Update time ->
                let
                    updatedPaddle =
                        paddle |> updatePosition |> checkBounds

                    updatedBall =
                        ball
                            |> updatePosition
                            |> checkBounds
                in
                    ( handleCollisions
                        { model
                            | paddle = updatedPaddle
                            , ball = updatedBall
                        }
                    , Cmd.none
                    )


subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Update
        ]



-- VIEW


px : Int -> String
px val =
    toString val ++ "px"


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
        [ div [] (List.map renderEntity model.bricks)
        , div
            [ style
                [ "position" => "absolute"
                , "top" => "500px"
                ]
            ]
            [ hr [] []
            , text <| toString model
            ]
        , renderEntity model.paddle
        , renderEntity model.ball
        ]
