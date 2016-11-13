module Main exposing (..)

import Html.App as App exposing (program)
import View exposing (view)
import Model exposing (initialModel, Msg, Model, defaultScreenDimensions)
import Update exposing (update, subscriptions)
import Ports exposing (getHighScore)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel defaultScreenDimensions, getHighScore () )
