module View exposing (..)

import Html exposing (Html, button, div, text, hr)
import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)
import Model exposing (..)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


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