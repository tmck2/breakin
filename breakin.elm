module Main exposing (..)

import Html exposing (Html, button, div, text, hr)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Paddle =
    { pos : ( Int, Int )
    , size : ( Int, Int )
    , color : String
    }


type alias Brick =
    { id : Int
    , pos : ( Int, Int )
    , size : ( Int, Int )
    , color : String
    }


type alias Model =
    { paddle : Paddle
    , bricks : List Brick
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


createBrick rowNum colNum color =
    { id = (rowNum + 1) * (colNum + 1)
    , pos = ( 5 + 5 * colNum + colNum * 40, 20 * rowNum + 5 + 5 * rowNum )
    , size = ( 40, 20 )
    , color = color
    }


model : Model
model =
    { paddle = { pos = ( 160, 400 ), size = ( 120, 20 ), color = "DarkGray" }
    , bricks =
        [0..5]
            |> List.concatMap (\i -> createRow i i 10)
    }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


px : Int -> String
px val =
    toString val ++ "px"


renderPaddle : Paddle -> Html a
renderPaddle paddle =
    let
        ( x, y ) =
            paddle.pos

        ( sx, sy ) =
            paddle.size
    in
        div
            [ style
                [ "position" => "absolute"
                , "left" => px x
                , "top" => px y
                , "width" => px sx
                , "height" => px sy
                , "background-color" => paddle.color
                , "display" => "inline-block"
                ]
            ]
            []


renderBrick : Brick -> Html a
renderBrick brick =
    let
        ( x, y ) =
            brick.pos

        ( sx, sy ) =
            brick.size
    in
        div
            [ style
                [ "position" => "absolute"
                , "left" => px x
                , "top" => px y
                , "width" => px sx
                , "height" => px sy
                , "background-color" => brick.color
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
        [ div [] (List.map renderBrick model.bricks)
        , div
            [ style
                [ "position" => "absolute"
                , "top" => "500px"
                ]
            ]
            [ hr [] []
            , text <| toString model
            ]
        , renderPaddle model.paddle
        ]
