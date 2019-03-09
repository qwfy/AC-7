module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Http

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
  { error: Error
  }

initModel : Model
initModel =
  { error = NoError
  }

type Msg
  = Enroll

type Error
  = NoError
  | HttpError Http.Error

-- TODO @incomplete: make sure there is a leading slash in the path
-- TODO @incomplete: read this port from a config file
-- TODO @incomplete: do not hard code the ip address and the port
pathToUrl : String -> String
pathToUrl path = "http://127.0.0.1:3000" ++ path

-- Note: Consider clear the error on every message.
-- TODO @incomplete: find another place to put the error?
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Html Msg
view model =
  div [] [viewError model.error]

viewError : Error -> Html Msg
viewError error =
  case error of
    NoError -> div [] []
    HttpError _ ->
      -- TODO @incomplete: More detailed error message
      div [] [text "network error"]
