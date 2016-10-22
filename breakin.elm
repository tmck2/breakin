module Main exposing (..)

import Html exposing (Html, button, div, text, hr)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


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
    Brick


model : Model
model =
    { id = 0, pos = ( 0, 0 ), size = ( 40, 20 ), color = "red" }



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


brick : Brick -> Html Msg
brick brick =
    div
        [ style
            [ ( "width", toString (fst brick.size) ++ "px" )
            , ( "height", toString (snd brick.size) ++ "px" )
            , ( "background-color", brick.color )
            ]
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ brick model
        , hr [] []
        , text <| toString model
        ]
