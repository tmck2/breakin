module Levels exposing (..)

import Model exposing (..)
import String
import Array


level1 =
    bricksFromCharMap
        [ ".........."
        , ".........."
        , "..YYYYYY.."
        , ".YY.YY.YY."
        , ".YYYYYYYY."
        , ".YYYYYYYY."
        , ".YY.YY.YY."
        , ".YYY..YYY."
        , "..YYYYYY.."
        , ".........."
        , ".........."
        ]


level2 =
    bricksFromCharMap
        [ ".........."
        , ".........."
        , "..RR..RR.."
        , ".RRRRRRRR."
        , ".RRRRRRRR."
        , ".RRRRRRRR."
        , "..RRRRRR.."
        , "..RRRRRR.."
        , "....RR...."
        , ".........."
        , ".........."
        ]


level3 =
    bricksFromCharMap
        [ ".........."
        , ".........."
        , "..G....G.."
        , "...G..G..."
        , "..GGGGGG.."
        , ".GG.GG.GG."
        , ".GGGGGGGG."
        , "G.G....G.G"
        , "G.GG..GG.G"
        , ".........."
        , ".........."
        ]


levels =
    Array.fromList [ level1, level2, level3 ]


mapCharToColor ch =
    case ch of
        'Y' ->
            "yellow"

        'R' ->
            "rgb(230, 52, 116)"

        'W' ->
            "white"

        'G' ->
            "rgb(17, 167, 72)"

        _ ->
            "gray"


bricksFromCharMap charMap =
    (charMap
        |> List.map2 (,) [0..11]
        |> List.map (\( row, str ) -> ( row, List.indexedMap (,) (String.toList str) ))
        |> List.concatMap (\( row, cols ) -> cols |> List.map (\( col, ch ) -> ( row, col, ch )))
        |> List.filter (\( row, col, ch ) -> ch /= '.')
        |> List.map (\( row, col, ch ) -> brick row col (mapCharToColor ch))
    )
