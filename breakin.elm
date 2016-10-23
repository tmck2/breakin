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


type alias Entity =
    { id : Int
    , pos : ( Int, Int )
    , size : ( Int, Int )
    , vel : ( Int, Int )
    , color : String
    }


type alias Model =
    { paddle : Entity
    , bricks : List Entity
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
    { id = (rowNum + 1) * (colNum + 1)
    , pos = ( 5 + 5 * colNum + colNum * 40, 20 * rowNum + 5 + 5 * rowNum )
    , size = ( 40, 20 )
    , vel = ( 0, 0 )
    , color = color
    }


init : ( Model, Cmd Msg )
init =
    ( { paddle = { id = 1, pos = ( 160, 400 ), size = ( 120, 20 ), vel = ( 0, 0 ), color = "DarkGray" }
      , bricks =
            [0..5]
                |> List.concatMap (\i -> createRow i i 10)
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = KeyUp KeyCode
    | KeyDown KeyCode
    | Update Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            let
                paddle =
                    model.paddle
            in
                if keyCode == 37 then
                    ( { model | paddle = { paddle | vel = ( -4, 0 ) } }, Cmd.none )
                else if keyCode == 39 then
                    ( { model | paddle = { paddle | vel = ( 4, 0 ) } }, Cmd.none )
                else
                    ( model, Cmd.none )

        KeyUp keyCode ->
            let
                paddle =
                    model.paddle
            in
                ( { model | paddle = { paddle | vel = ( 0, 0 ) } }, Cmd.none )

        Update time ->
            let
                paddle =
                    model.paddle

                ( vx, vy ) =
                    paddle.vel

                ( x, y ) =
                    paddle.pos
            in
                ( { model | paddle = { paddle | pos = ( x + vx, y ) } }, Cmd.none )


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
        ]
