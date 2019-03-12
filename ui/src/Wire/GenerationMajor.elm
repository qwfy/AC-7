module Wire.GenerationMajor exposing (..)

import Json.Decode
import Json.Decode.Pipeline

import Wire.Genome

type alias Species =
    { species_sn : Int
    , genomes : List Wire.Genome.T
    }

decodeSpecies : Json.Decode.Decoder Species
decodeSpecies =
    Json.Decode.succeed Species
        |> Json.Decode.Pipeline.required "species_sn" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "genomes" (Json.Decode.list Wire.Genome.decodeT)

type alias Generation =
  { generation_sn : Int
  , species: List Species
  }

decodeGeneration : Json.Decode.Decoder Generation
decodeGeneration =
    Json.Decode.succeed Generation
        |> Json.Decode.Pipeline.required "generation_sn_" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "species_" (Json.Decode.list decodeSpecies)
