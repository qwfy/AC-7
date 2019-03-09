module Wire.RunInfo exposing (..)

import Json.Decode
import Json.Decode.Pipeline

type alias T =
    { run_id : String
    , species_sns : List Int
    , generation_sns : List Int
    }

decodeT : Json.Decode.Decoder T
decodeT =
    Json.Decode.succeed T
        |> Json.Decode.Pipeline.required "run_id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "species_sns" (Json.Decode.list Json.Decode.int)
        |> Json.Decode.Pipeline.required "generation_sns" (Json.Decode.list Json.Decode.int)
