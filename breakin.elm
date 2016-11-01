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
    ( initialModel, Cmd.none )
