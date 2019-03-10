port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, li, ol, table, tr, td, th, thead, tbody, span)
import Html.Events exposing (onClick)
import Html.Attributes as Attributes
import Http
import Url.Builder

import Json.Decode as Decode
import Json.Encode as Encode
import Wire.Run
import Wire.RunInfo
import Wire.Genome
import Wire.GenerationMajor
import Wire.SpeciesMajor

port flushGenomeGraphs : String -> Cmd msg

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = (\model_ -> Sub.none)
    }

-- The argument flags_ is unused, the concrete type String is there
-- to make the elm compiler happy.
init : String -> (Model, Cmd Msg)
init flags_ = (initModel, Cmd.none)

type alias Model =
  { error : Error
  , runs : List Wire.Run.T
  , query : Maybe Query
  , genomes : List Wire.Genome.T
  }

initModel : Model
initModel =
  { error = NoError
  , runs = []
  , query = Nothing
  , genomes = []
  }

type Selection a
  = Selected a
  | Unselected a

type Select a
  = ToggleOne a
  | SelectAll
  | SelectNone

type alias Query =
  { runId : RunId
  , speciesSns: List (Selection SpeciesSn)
  , generationSns: List (Selection GenerationSn)
  , orderByOriginalFitness : Order
  , displayMethod : DisplayMethod
  }

type DisplayMethod
  = SpeciesMajor    -- clollect the same species together, this shows the evolution of a single species
  | GenerationMajor -- collect the same generation together, this shows the different species in a single generation
  | UndefinedDisplayMethod

type QueryResult
  = QR_SpeciesMajor (List Wire.SpeciesMajor.Species)
  | QR_GenerationMajor (List Wire.GenerationMajor.Generation)
  | QR_UndefinedDisplayMethod (List Wire.Genome.T)

type Order = Asc | Desc | UndefinedOrder

type alias RunId = String
type alias SpeciesSn = Int
type alias GenerationSn = Int

type Error = NoError | HttpError Http.Error

type Msg
  = LoadAllRuns
  | LoadedAllRuns (Result Http.Error (List Wire.Run.T))
  | LoadRunInfo RunId
  | LoadedRunInfo (Result Http.Error Wire.RunInfo.T)
  | RemoveError
  | SelectSpecies (Select SpeciesSn)
  | SelectGeneration (Select GenerationSn)
  | LoadQuery
  | LoadedQuery (Result Http.Error QueryResult)
  | ChangeOriginalFitnessOrder Order
  | ChangeDisplayMethod DisplayMethod

-- TODO @incomplete: make sure there is a leading slash in the path
-- TODO @incomplete: read this port from a config file
-- TODO @incomplete: do not hard code the ip address and the port
makeUrl : List String -> List Url.Builder.QueryParameter -> String
makeUrl paths params =
  Url.Builder.crossOrigin
    "http://127.0.0.1:3000" paths params

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RemoveError ->
      ({model|error=NoError}, Cmd.none)
    LoadAllRuns ->
      let cmd = Http.get
            { url = makeUrl ["run_info"] [Url.Builder.string "order" "time_stopped.desc,time_started.desc"]
            , expect = Http.expectJson LoadedAllRuns (Decode.list Wire.Run.decodeT)
            }
      in (model, cmd)
    LoadedAllRuns runsRes ->
      case runsRes of
        Err err ->
          ({model|error=HttpError err}, Cmd.none)
        Ok runs ->
          ({model|runs=runs}, Cmd.none)
    LoadRunInfo runId ->
      let cmd = Http.request
            { method = "GET"
            , headers = [pgRestSingleObjectHeader]
            , url = makeUrl ["run_info"]
                      [Url.Builder.string "run_id" ("eq." ++ runId)]
            , body = Http.emptyBody
            , expect = Http.expectJson LoadedRunInfo (Wire.RunInfo.decodeT)
            , timeout = Nothing
            , tracker = Nothing
            }
      in (model, cmd)
    LoadedRunInfo runInfoRes ->
      case runInfoRes of
        Err err ->
          ({model|error=HttpError err}, Cmd.none)
        Ok runInfo ->
          let newQuery =
                { runId = runInfo.run_id
                , speciesSns = List.map Selected runInfo.species_sns
                , generationSns = List.map Selected runInfo.generation_sns
                , orderByOriginalFitness = Desc
                , displayMethod = UndefinedDisplayMethod
                }
              newModel = {model|query=Just newQuery}
          in (newModel, Cmd.none)
    SelectSpecies select ->
      case model.query of
        Nothing ->
          (model, Cmd.none)
        Just query ->
          let newQuery = {query|speciesSns=runSelect query.speciesSns select}
          in ({model|query=Just newQuery}, Cmd.none)
    SelectGeneration select ->
      case model.query of
        Nothing ->
          (model, Cmd.none)
        Just query ->
          let newQuery = {query|generationSns=runSelect query.generationSns select}
          in ({model|query=Just newQuery}, Cmd.none)
    LoadQuery ->
      case model.query of
        Nothing -> (model, Cmd.none)
        Just query ->
          let speciesSns = extractSelected query.speciesSns
              generationSns = extractSelected query.generationSns
              cmd = case query.displayMethod of
                UndefinedDisplayMethod ->
                  let orderByOriginalFitness = case query.orderByOriginalFitness of
                        UndefinedOrder -> []
                        Asc -> [Url.Builder.string "order" "original_fitness.asc"]
                        Desc -> [Url.Builder.string "order" "original_fitness.desc"]
                  in Http.get
                       { url = makeUrl ["population"] <|
                                 [ Url.Builder.string "run_id" ("eq." ++ query.runId)
                                 , Url.Builder.string "generation_sn" <| pgRestInInts generationSns
                                 , Url.Builder.string "species_sn" <| pgRestInInts speciesSns
                                 ] ++ orderByOriginalFitness
                       , expect = Http.expectJson LoadedQuery (Decode.map QR_UndefinedDisplayMethod <| Decode.list Wire.Genome.decodeT)
                       }
                GenerationMajor ->
                  let decoder = Decode.map QR_GenerationMajor (Decode.list Wire.GenerationMajor.decodeGeneration)
                  in requestMajoredQuery query speciesSns generationSns "population_generation_major" decoder
                SpeciesMajor ->
                  let decoder = Decode.map QR_SpeciesMajor (Decode.list Wire.SpeciesMajor.decodeSpecies)
                  in requestMajoredQuery query speciesSns generationSns "population_species_major" decoder
          in (model, cmd)
    LoadedQuery queryRes ->
      case queryRes of
        Err err -> ({model|error=HttpError err}, Cmd.none)
        Ok res ->
          case res of
            QR_UndefinedDisplayMethod genomes ->
              let graphString = String.concat <| List.map (\x -> x.graph) genomes
              in ({model|genomes=genomes}, flushGenomeGraphs graphString)
            QR_GenerationMajor _ ->
              -- TODO @incomplete: finish this
              (model, Cmd.none)
            QR_SpeciesMajor _ ->
              -- TODO @incomplete: finish this
              (model, Cmd.none)
    ChangeOriginalFitnessOrder order ->
      case model.query of
        Nothing -> (model, Cmd.none)
        Just query ->
          let newQuery = {query|orderByOriginalFitness=order}
          in ({model|query=Just newQuery}, Cmd.none)
    ChangeDisplayMethod displayMethod ->
      case model.query of
        Nothing -> (model, Cmd.none)
        Just query ->
          let newQuery = {query|displayMethod=displayMethod}
          in ({model|query=Just newQuery}, Cmd.none)

pgRestInInts : List Int -> String
pgRestInInts xs =
  let commas = String.join "," (List.map String.fromInt xs)
  in String.concat ["in.", "(", commas, ")"]

pgRestSingleObjectHeader : Http.Header
pgRestSingleObjectHeader = Http.header "Accept" "application/vnd.pgrst.object+json"

unwrapSelection : Selection a -> a
unwrapSelection x =
  case x of
    Selected y -> y
    Unselected y -> y

runSelect : List (Selection a) -> Select a -> List (Selection a)
runSelect xs select =
  case select of
    SelectAll -> List.map unwrapSelection xs |> List.map Selected
    SelectNone -> List.map unwrapSelection xs |> List.map Unselected
    ToggleOne target ->
      let toggle source =
            if unwrapSelection source == target
            then case source of
              Selected y -> Unselected y
              Unselected y -> Selected y
            else source
      in List.map toggle xs

view : Model -> Html Msg
view model =
  div []
    [ viewError model.error
    , viewControl model
    ]


viewControl : Model -> Html Msg
viewControl model =
  div []
    [ viewLoadRuns
    , viewRuns model.runs
    , viewQuery model.query
    ]

viewLoadRuns : Html Msg
viewLoadRuns =
  button [onClick LoadAllRuns] [text "load all runs"]

viewRuns : List Wire.Run.T -> Html Msg
viewRuns runs =
  let headers = thead []
        [ th [] [text "run id"]
        , th [] [text "time started"]
        , th [] [text "time stopped"]
        ]
      viewRun run =
        tr []
          [ td [] [button [onClick (LoadRunInfo run.run_id)] [text run.run_id]]
          , td [] [text run.time_started]
          , td [] [text run.time_stopped]
          ]
  in if List.isEmpty runs
     then table [] []
     else table [Attributes.id "run-table"] [headers, tbody [] (List.map viewRun runs)]

viewError : Error -> Html Msg
viewError error =
  case error of
    NoError -> div [] []
    HttpError err ->
      let hint = case err of
            Http.BadUrl url ->
              "BadUrl: you did not provide a valid URL. " ++ url
            Http.Timeout ->
              "Timeout: it took too long to get a response"
            Http.NetworkError->
              "NetworkError: cannot connect to the remote host"
            Http.BadStatus code ->
              "BadStatus (" ++ String.fromInt code ++ "): the status code indicates failure"
            Http.BadBody detail ->
              "BadBody: the body of the response was something unexpected. " ++ detail
          close = button [onClick RemoveError] [text "close this message"]
      in div [Attributes.class "error"] [text hint, close]

viewQuery : Maybe Query -> Html Msg
viewQuery query1 =
  case query1 of
    Nothing -> div [] []
    Just query ->
      div [Attributes.id "query-container"]
        [ div [] [text <| "exploring run id: " ++ query.runId]
        , viewSns String.fromInt query.generationSns SelectGeneration "please select the interested generations" "generation-sn-container"
        , viewSns String.fromInt query.speciesSns SelectSpecies "please select the interested species" "species-sn-container"
        , viewRunQuery query
        ]

viewSns : (sn -> String) -> List (Selection sn) -> (Select sn -> Msg) -> String -> String -> Html Msg
viewSns snToString selections toMsg hint containerId =
  let allButton = button [onClick (toMsg SelectAll)] [text "select all"]
      noneButton = button [onClick (toMsg SelectNone) ] [text "select none"]
      hintElem = div [] [text hint]
  in div [Attributes.id containerId] (hintElem :: allButton :: noneButton :: List.map (viewSn snToString toMsg) selections)

viewSn : (sn -> String) -> (Select sn -> Msg) -> Selection sn -> Html Msg
viewSn snToString toMsg selection =
  let sn = unwrapSelection selection
      class = case selection of
        Selected _ -> "sn-selected"
        Unselected _ -> "sn-unselected"
  in button
      [ onClick (toMsg <| ToggleOne sn)
      , Attributes.class class
      ]
      [ text <| snToString sn
      ]

viewRunQuery : Query -> Html Msg
viewRunQuery query =
  let queryButton = button [onClick LoadQuery, Attributes.id "query-button"] [text "query"]
      orderByOFSelected x =
        Attributes.class <|
          if x == query.orderByOriginalFitness
          then "order-by-original-fitness-selected"
          else "order-by-original-fitness-unselected"
      orderByOriginalFitness =
        div [Attributes.id "order-by-fitness-container"]
          [ span [Attributes.attribute "display" "inline-block"] [text "order by original fitness:"]
          , button [onClick <| ChangeOriginalFitnessOrder Desc, orderByOFSelected Desc] [text "desc"]
          , button [onClick <| ChangeOriginalFitnessOrder Asc, orderByOFSelected Asc] [text "asc"]
          , button [onClick <| ChangeOriginalFitnessOrder UndefinedOrder, orderByOFSelected UndefinedOrder] [text "undefined order"]
          ]
      dmSelected x =
        Attributes.class <|
          if x == query.displayMethod
          then "display-method-selected"
          else "display-method-unselected"
      displayMethod =
        div [Attributes.id "display-method-container"]
          [ span [Attributes.attribute "display" "inline-block"] [text "display method:"]
          , button [onClick <| ChangeDisplayMethod SpeciesMajor, dmSelected SpeciesMajor] [text "species major"]
          , button [onClick <| ChangeDisplayMethod GenerationMajor, dmSelected GenerationMajor] [text "generation major"]
          , button [onClick <| ChangeDisplayMethod UndefinedDisplayMethod, dmSelected UndefinedDisplayMethod] [text "undefined display method"]
          ]
  in div [Attributes.class "run-query-container"]
       [ displayMethod
       , orderByOriginalFitness
       , queryButton
       ]


extractSelected : List (Selection sn) -> List sn
extractSelected selections =
  let selected sx =
        case sx of
          Selected x -> Just x
          Unselected _ -> Nothing
      allSelected = List.filterMap selected selections
  in if List.isEmpty allSelected
     then List.map unwrapSelection selections
     else allSelected


requestMajoredQuery : Query -> List SpeciesSn -> List GenerationSn -> String -> Decode.Decoder QueryResult -> Cmd Msg
requestMajoredQuery query speciesSns generationSns procedureName decoder =
  let order_method = case query.orderByOriginalFitness of
        Asc -> "asc"
        Desc -> "desc"
        UndefinedOrder -> "undefined_order"
  in Http.post
   { url = makeUrl ["rpc", procedureName] []
   , expect = Http.expectJson LoadedQuery decoder
   , body = Http.jsonBody <| Encode.object
       [ ("_run_id", Encode.string query.runId)
       , ("_species_sns", Encode.list Encode.int speciesSns)
       , ("_generation_sns", Encode.list Encode.int generationSns)
       , ("_order_method", Encode.string order_method)
       ]
   }
