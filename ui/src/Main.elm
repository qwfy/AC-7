module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, li, ol, table, tr, td, th)
import Html.Events exposing (onClick)
import Html.Attributes as Attributes
import Http
import Url.Builder

import Json.Decode as Decode
import Wire.Run
import Wire.RunInfo

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
  }

type alias Query =
  { timeline: Timeline
  , runId : RunId
  , speciesIds: List SpeciesId
  , generationIds: List GenerationId
  }

type Timeline = ByGeneration
type alias RunId = String
type alias SpeciesId = Int
type alias GenerationId = Int

initModel : Model
initModel =
  { error = NoError
  , runs = []
  , query = Nothing
  }

type Error = NoError | HttpError Http.Error

type Msg
  = LoadAllRuns
  | LoadedAllRuns (Result Http.Error (List Wire.Run.T))
  | ChangeTimeline Timeline
  | LoadRunInfo RunId
  | LoadedRunInfo (Result Http.Error Wire.RunInfo.T)
  | RemoveError

-- TODO @incomplete: make sure there is a leading slash in the path
-- TODO @incomplete: read this port from a config file
-- TODO @incomplete: do not hard code the ip address and the port
makeUrl : List String -> List Url.Builder.QueryParameter -> String
makeUrl paths params =
  Url.Builder.crossOrigin
    "http://127.0.0.1:3000" paths params

-- Note: Consider clear the error on every message.
-- TODO @incomplete: find another place to put the error?
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
            , headers = [Http.header "Accept" "application/vnd.pgrst.object+json"]
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
                { timeline = case model.query of
                    Nothing -> ByGeneration
                    Just q -> q.timeline
                , runId = runInfo.run_id
                , speciesIds = runInfo.species_sns
                , generationIds = runInfo.generation_sns
                }
              newModel = {model|query=Just newQuery}
          in (newModel, Cmd.none)
    ChangeTimeline timeline ->
      case model.query of
        Nothing -> (model, Cmd.none)
        Just query ->
          let newModel = {model|query=Just {query|timeline=timeline}}
          in (newModel, Cmd.none)


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
  let headers = th []
        [ td [] [text "run id"]
        , td [] [text "time started"]
        , td [] [text "time stopped"]
        ]
      viewRun run =
        tr []
          [ td [onClick (LoadRunInfo run.run_id)] [text run.run_id]
          , td [] [text run.time_started]
          , td [] [text run.time_stopped]
          ]
  in table [] (List.map viewRun runs)

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
      let speciesAndGenerations =
            case query.timeline of
              ByGeneration ->
                [viewGenerationIds query.generationIds, viewSpeciesIds query.speciesIds]
      in div []
           ([ viewTimeline query.timeline
            , div [] [text query.runId]
            ] ++ speciesAndGenerations)

viewSpeciesIds : List SpeciesId -> Html Msg
viewSpeciesIds speciesIds =
  let allButton = button [] [text "all"]
  in div [] (allButton :: List.map viewSpeciesId speciesIds)

viewSpeciesId : SpeciesId -> Html Msg
viewSpeciesId speciesId =
  button [] [text <| String.fromInt speciesId]

viewGenerationIds : List GenerationId -> Html Msg
viewGenerationIds generationIds =
  let allButton = button [] [text "all"]
  in div [] (allButton :: List.map viewGenerationId generationIds)

viewGenerationId : GenerationId -> Html Msg
viewGenerationId generationId =
  button [] [text <| String.fromInt generationId]


viewTimeline : Timeline -> Html Msg
viewTimeline timeline =
  div []
    -- TODO @incomplete: indicator of selection
    [ button [onClick (ChangeTimeline ByGeneration)] [text "by generation"]
    ]
