module Main exposing (..)

import Html.App as App
import View exposing (..)
import Model exposing (..)
import Update exposing (..)
import Levels exposing (..)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , paddle =
            { id = Paddle
            , x = 160
            , y = 400
            , sx = 100
            , sy = 20
            , vx = 0
            , vy = 0
            , color = "#ddd"
            }
      , bricks =
            level1
      , ball =
            { id = Ball
            , x = 210
            , y = 380
            , sx = 20
            , sy = 20
            , vx = 0
            , vy = 0
            , color = "#bbb"
            }
      , state = Title
      , lives = 3
      , score = 0
      , level = 0
      }
    , Cmd.none
    )
