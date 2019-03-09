module Wire.Genome exposing (..)

import Json.Decode
import Json.Decode.Pipeline

type alias T =
    { run_id : String
    , generation_sn : Int
    , species_sn : Int
    , genome_id : String
    , original_fitness : Float
    , graph : String
    }

decodeT : Json.Decode.Decoder T
decodeT =
    Json.Decode.succeed T
        |> Json.Decode.Pipeline.required "run_id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "generation_sn" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "species_sn" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "genome_id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "original_fitness" (Json.Decode.float)
        |> Json.Decode.Pipeline.required "graph" (Json.Decode.string)
