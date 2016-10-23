module Main exposing (..)

import Html exposing (Html, button, div, text, hr)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (..)
import AnimationFrame
import Time exposing (Time)


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
    , pos : ( Int, Int )
    , size : ( Int, Int )
    , vel : ( Int, Int )
    , color : String
    }


type alias Model =
    { paddle : Entity
    , bricks : List Entity
    , ball : Entity
    }


color i =
    case i % 3 of
        0 ->
            "rgb(92, 199, 42)"

        1 ->
            "rgb(35, 139, 214)"

        2 ->
            "rgb(205, 112, 2)"

        _ ->
            "black"


createRow seed rowNum len =
    if len == 0 then
        []
    else
        (createBrick rowNum (len - 1) (color seed)) :: (createRow (rowNum * seed + len) rowNum (len - 1))


createBrick : Int -> Int -> String -> Entity
createBrick rowNum colNum color =
    { id = Brick <| (rowNum + 1) * (colNum + 1)
    , pos = ( 5 + 5 * colNum + colNum * 40, 20 * rowNum + 5 + 5 * rowNum )
    , size = ( 40, 20 )
    , vel = ( 0, 0 )
    , color = color
    }


init : ( Model, Cmd Msg )
init =
    ( { paddle = { id = Paddle, pos = ( 160, 400 ), size = ( 120, 20 ), vel = ( 0, 0 ), color = "rgb(57, 60, 68)" }
      , bricks =
            [0..5]
                |> List.concatMap (\i -> createRow i i 10)
      , ball = { id = Ball, pos = ( 160, 200 ), size = ( 20, 20 ), vel = ( 4, -4 ), color = "rgb(57, 60, 68)" }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyUp KeyCode
    | KeyDown KeyCode
    | Update Time


updateEntity entity =
    let
        ( vx, vy ) =
            entity.vel

        ( x, y ) =
            entity.pos
    in
        { entity | pos = ( x + vx, y + vy ) }


checkCollision entity =
    let
        ( vx, vy ) =
            entity.vel

        ( x, y ) =
            entity.pos
    in
        case entity.id of
            Paddle ->
                if x < 0 then
                    { entity | pos = ( 0, y ) }
                else if x > 330 then
                    { entity | pos = ( 330, y ) }
                else
                    entity

            Ball ->
                if y < 0 then
                    { entity | pos = ( x, 0 ), vel = ( vx, -vy ) }
                else if x > 430 then
                    { entity | pos = ( 430, y ), vel = ( -vx, vy ) }
                else if x < 0 then
                    { entity | pos = ( 0, y ), vel = ( -vx, vy ) }
                else
                    entity

            _ ->
                entity


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
                    ( { model | paddle = { paddle | vel = ( -4, 0 ) } }, Cmd.none )
                else if keyCode == 39 then
                    ( { model | paddle = { paddle | vel = ( 4, 0 ) } }, Cmd.none )
                else
                    ( model, Cmd.none )

            KeyUp keyCode ->
                ( { model | paddle = { paddle | vel = ( 0, 0 ) } }, Cmd.none )

            Update time ->
                let
                    updatedPaddle =
                        paddle |> updateEntity |> checkCollision

                    updatedBall =
                        ball |> updateEntity |> checkCollision
                in
                    ( { model
                        | paddle = updatedPaddle
                        , ball = updatedBall
                      }
                    , Cmd.none
                    )


subscriptions model =
    Sub.batch
        [ downs KeyDown
        , ups KeyUp
        , AnimationFrame.diffs Update
        ]



-- VIEW


px : Int -> String
px val =
    toString val ++ "px"


renderEntity : Entity -> Html a
renderEntity entity =
    let
        ( x, y ) =
            entity.pos

        ( sx, sy ) =
            entity.size
    in
        div
            [ style
                [ "position" => "absolute"
                , "left" => px x
                , "top" => px y
                , "width" => px sx
                , "height" => px sy
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
