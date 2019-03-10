module Wire.SpeciesMajor exposing (..)

import Json.Decode
import Json.Decode.Pipeline

import Wire.Genome

type alias Generation =
    { generation_sn : Int
    , genomes : List Wire.Genome.T
    }

decodeGeneration : Json.Decode.Decoder Generation
decodeGeneration =
    Json.Decode.succeed Generation
        |> Json.Decode.Pipeline.required "generation_sn" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "genomes" (Json.Decode.list Wire.Genome.decodeT)

type alias Species =
  { run_id : String
  , species_sn : Int
  , generations: List Generation
  }

decodeSpecies : Json.Decode.Decoder Species
decodeSpecies =
    Json.Decode.succeed Species
        |> Json.Decode.Pipeline.required "run_id_" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "species_sn_" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "generations_" (Json.Decode.list decodeGeneration)
