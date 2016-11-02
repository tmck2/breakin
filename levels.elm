module Levels exposing (..)

import Array


level1 =
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
