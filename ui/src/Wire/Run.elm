module Wire.Run exposing (..)

import Json.Decode
import Json.Decode.Pipeline

type alias T =
    { run_id : String
    , time_started : String
    , time_stopped : String
    }

decodeT : Json.Decode.Decoder T
decodeT =
    Json.Decode.succeed T
        |> Json.Decode.Pipeline.required "run_id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "time_started" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "time_stopped" (Json.Decode.string)
