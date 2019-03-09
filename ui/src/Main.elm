module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Http
import Url.Builder

import Json.Decode as Decode
import Wire.Run

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
  { messages: List Message
  , run : Maybe RunId
  , species: Maybe SpeciesId
  , generation: Maybe GenerationId
  }

type alias RunId = String
type alias SpeciesId = String
type alias GenerationId = String

initModel : Model
initModel =
  { messages = []
  , run = Nothing
  , species = Nothing
  , generation = Nothing
  }

type Msg
  = LoadAllRuns
  | LoadedAllRuns (Result Http.Error (List Wire.Run.Run))

type Message
  = Progress String
  | FinishProgress String

-- TODO @incomplete: make sure there is a leading slash in the path
-- TODO @incomplete: read this port from a config file
-- TODO @incomplete: do not hard code the ip address and the port
makeUrl : List String -> List Url.Builder.QueryParameter -> String
makeUrl paths params =
  Url.Builder.crossOrigin
    "http://127.0.0.1:3000" paths params

withMessage : Message -> Model -> Model
withMessage msg model =
  {model | messages = msg :: model.messages}

-- Note: Consider clear the error on every message.
-- TODO @incomplete: find another place to put the error?
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadAllRuns ->
      let cmd = Http.get
            { url = makeUrl ["run"] [Url.Builder.string "order" "time_stopped.desc,time_started.desc"]
            , expect = Http.expectJson LoadedAllRuns (Decode.list Wire.Run.decodeRun)
            }
          newModel = withMessage (Progress "loading all runs") model
      in (newModel, cmd)
    LoadedAllRuns runsRes ->
      case runsRes of
        Err _ ->
          let newModel = withMessage (Progress "network error") model
          in (newModel, Cmd.none)
        Ok runs ->
          let newModel = withMessage (FinishProgress "loaded all runs") model
          in (newModel, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ viewMessages model.messages
    , viewControl model
    ]


viewControl : Model -> Html Msg
viewControl model =
  div []
    [ viewLoadRuns
    -- , viewRuns
    -- , viewSpecies
    -- , viewGenerations
    ]

viewLoadRuns : Html Msg
viewLoadRuns =
  button [onClick LoadAllRuns] [text "load all runs"]

viewMessages : List Message -> Html Msg
viewMessages messages =
  div [] (List.map viewMessage messages)

viewMessage : Message -> Html Msg
viewMessage message =
  case message of
    Progress msg ->
      div [] [text msg]
    FinishProgress msg ->
      div [] [text msg]
