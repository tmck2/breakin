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


renderEntity : List ( String, String ) -> Entity -> Html a
renderEntity styles entity =
    div
        [ class "entity"
        , style
            (List.append styles
                [ "left" => px entity.x
                , "top" => px entity.y
                , "width" => px entity.sx
                , "height" => px entity.sy
                , "background-color" => entity.color
                ]
            )
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


titleView : Model -> Html a
titleView model =
    div [ id "title-container" ]
        [ div
            [ id "title-inner-container" ]
            [ div [ id "title", style [ "font-size" => "100px" ] ] [ text "Breakin" ]
            , div [] [ text "Press any key to continue..." ]
            ]
        ]


renderInstructions : Model -> Html a
renderInstructions model =
    if model.state == Serving then
        div [ id "instructions" ] [ text "Press ctrl to serve" ]
    else
        []


view : Model -> Html Msg
view model =
    case model.state of
        Title ->
            titleView model

        _ ->
            div []
                [ div
                    [ id "playing-field" ]
                    [ renderScore model.score
                    , div [] (List.map (renderEntity []) model.bricks)
                    , renderEntity [ "border-radius" => px 10 ] model.paddle
                    , renderEntity [ "border-radius" => px 10 ] model.ball
                    , renderLives model.lives
                    , renderInstructions model
                    ]
                , div [ style [ "top" => "500px" ] ]
                    [ hr [] [], div [] [ text (toString model) ] ]
                ]
