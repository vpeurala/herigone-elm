module Shuffler exposing (Model, Msg, update)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Basics.Extra as Extra exposing (never)
import Time exposing (Time)
import Task


type Msg
    = AskTime
    | GetTime Time


type alias Model =
    { time : Maybe Time }


init : ( Model, Cmd Msg )
init =
    ( { time = Nothing }, Task.perform never GetTime Time.now )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AskTime ->
            ( model, Task.perform never GetTime Time.now )

        GetTime time' ->
            ( { model | time = (Just time') }, Cmd.none )


view : Model -> Html Msg
view { time } =
    div [] [ toString time |> text ]


main =
    App.program
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
