module View exposing (..)

import Html exposing (Html, button, div, text, hr, br)
import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)
import Model exposing (..)
import TouchEvents


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


renderLives : Model -> Html a
renderLives { lives, screenWidth, screenHeight } =
    div
        [ id "lives-container"
        , style [ ( "left", px <| screenWidth - 3.5 * (ballSize screenHeight) ) ]
        ]
        [ div [ style [] ]
            (List.map
                (\n ->
                    div
                        [ class "life"
                        , style
                            [ "display" => "inline-block"
                            , "width" => px (ballSize screenHeight)
                            , "height" => px (ballSize screenHeight)
                            ]
                        ]
                        []
                )
                (List.range 1 lives) 
            )
        ]


titleView : Model -> Html Msg
titleView model =
    div
        [ id "title-container"
        , style
            [ "width" => px model.screenWidth
            , "height" => px model.screenHeight
            ]
        , TouchEvents.onTouchStart TouchDown
        , TouchEvents.onTouchEnd TouchUp
        ]
        [ div
            [ id "title-inner-container" ]
            [ div [ id "title", style [ "font-size" => "100px" ] ] [ text "Breakin" ]
            , div [] [ text "Press any key to continue..." ]
            ]
        ]


gameOverView : Model -> Html Msg
gameOverView model =
    div
        [ id "title-container"
        , style
            [ "width" => px model.screenWidth
            , "height" => px model.screenHeight
            ]
        , TouchEvents.onTouchStart TouchDown
        , TouchEvents.onTouchEnd TouchUp
        ]
        [ div
            [ id "title-inner-container" ]
            [ div [ id "title", style [ "font-size" => "100px" ] ] [ text "Game Over" ]
            , div [] [ text <| "Score: " ++ (toString model.score) ++ ", Best: " ++ (toString model.highScore) ]
            , br [] []
            , div [] [ text "Press any key to continue..." ]
            ]
        ]


renderInstructions : Model -> Html a
renderInstructions model =
    if model.state == Serving then
        div
            [ id "instructions"
            , style [ "top" => px (model.screenHeight - model.screenHeight / 4) ]
            ]
            [ text "Ctrl to serve. Left and right to move paddle." ]
    else
        div [] []


view : Model -> Html Msg
view ({ ball } as model) =
    case model.state of
        Title ->
            titleView model

        GameOver ->
            gameOverView model

        _ ->
            div
                [ TouchEvents.onTouchStart TouchDown
                , TouchEvents.onTouchEnd TouchUp
                ]
                [ div
                    [ id "playing-field"
                    , style
                        [ "width" => px model.screenWidth
                        , "height" => px model.screenHeight
                        ]
                    ]
                    [ renderScore model.score
                    , div [] (List.map (renderEntity []) model.bricks)
                    , renderEntity [ "border-radius" => px 10 ] model.paddle
                    , renderEntity [ "border-radius" => px 10 ] model.ball
                    , renderLives model
                    , renderInstructions model
                    ]
                  --, div [ style [ "top" => "500px" ] ]
                  --    [ hr [] [], div [] [ text (toString ( ball.x, ball.y, ball.vx, ball.vy )) ] ]
                ]
