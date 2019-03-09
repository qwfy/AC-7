module Wire.Run exposing (..)

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline

type alias Run =
    { run_id : String
    , time_started : String
    , time_stopped : String
    }

decodeRun : Json.Decode.Decoder Run
decodeRun =
    Json.Decode.succeed Run
        |> Json.Decode.Pipeline.required "run_id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "time_started" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "time_stopped" (Json.Decode.string)

encodeRun : Run -> Json.Encode.Value
encodeRun record =
    Json.Encode.object
        [ ("run_id",  Json.Encode.string <| record.run_id)
        , ("time_started",  Json.Encode.string <| record.time_started)
        , ("time_stopped",  Json.Encode.string <| record.time_stopped)
        ]
