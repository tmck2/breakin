port module Ports exposing (..)


port playSound : String -> Cmd msg


port saveState : String -> Cmd msg


port rewind : () -> Cmd msg


port fastforward : () -> Cmd msg


port updateModel : (String -> msg) -> Sub msg


port saveHighScore : Int -> Cmd msg


port getHighScore : () -> Cmd msg


port updateHighScore : (Int -> msg) -> Sub msg
