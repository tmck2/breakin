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


type alias Brick =
    { id : Int
    , pos : ( Int, Int )
    , size : ( Int, Int )
    , color : String
    }


type alias Model =
    { bricks : List Brick }


color i =
    case i % 3 of
        0 ->
            "red"

        1 ->
            "blue"

        2 ->
            "green"

        _ ->
            "black"


createRow seed rowNum len =
    if len == 0 then
        []
    else
        (createBrick rowNum len (color seed)) :: (createRow (rowNum * seed + len) rowNum (len - 1))


createBrick rowNum colNum color =
    { id = (rowNum + 1) * colNum
    , pos = ( 5 + 5 * colNum + colNum * 40, 20 * rowNum + 5 + 5 * rowNum )
    , size = ( 40, 20 )
    , color = color
    }


model : Model
model =
    { bricks =
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


renderBrick : Brick -> Html Msg
renderBrick brick =
    div
        [ style
            [ "position" => "absolute"
            , "left" => (toString (fst brick.pos) ++ "px")
            , "top" => (toString (snd brick.pos) ++ "px")
            , "width" => (toString (fst brick.size) ++ "px")
            , "height" => (toString (snd brick.size) ++ "px")
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
                , "top" => "400px"
                ]
            ]
            [ hr [] []
            , text <| toString model
            ]
        ]
