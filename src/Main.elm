module Main (..) where

import Char exposing (KeyCode, fromCode, isLower, isUpper)
import Debug exposing (..)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, tabindex, value)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp exposing (App, start)
import String exposing (fromChar, toUpper)
import Task exposing (Task)


allAssociations : List Association
allAssociations =
  [ { number = "0", word = "hai" }
  , { number = "1", word = "jää" }
  , { number = "2", word = "kuu" }
  , { number = "3", word = "luu" }
  , { number = "4", word = "maa" }
  , { number = "5", word = "puu" }
  , { number = "6", word = "rae" }
  , { number = "7", word = "suu" }
  , { number = "8", word = "täi" }
  , { number = "9", word = "voi" }
  , { number = "00", word = "hiha" }
  , { number = "01", word = "häjy" }
  , { number = "02", word = "hauki" }
  , { number = "03", word = "huilu" }
  , { number = "04", word = "haamu" }
  , { number = "05", word = "huopa" }
  , { number = "06", word = "hiiri" }
  , { number = "07", word = "huusi" }
  , { number = "08", word = "hauta" }
  , { number = "09", word = "haavi" }
  , { number = "10", word = "jauho" }
  ]


type alias Association =
  { number : String
  , word : String
  }


type Model
  = Initial
  | Running GameState
  | Paused GameState
  | Over GameState


type alias GameState =
  { done : List Association
  , left : List Association
  , current : Association
  , input : String
  , time : Int
  }


type Action
  = NoOp
  | StartOrPause
  | Input Char
  | Backspace


keyboard : Int -> Action
keyboard x =
  case x of
    32 ->
      StartOrPause

    8 ->
      Backspace

    222 ->
      Input 'Ä'

    186 ->
      Input 'Ö'

    c ->
      let
        ch =
          fromCode c
      in
        if isUpper ch then
          Input ch
        else
          NoOp


startGame : GameState
startGame =
  case allAssociations of
    [] ->
      Debug.crash "No associations!"

    x :: xs ->
      { done = []
      , left = xs
      , current = x
      , input = ""
      , time = 0
      }


init : ( Model, Effects Action )
init =
  ( Initial
  , Effects.none
  )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case ( action, model ) of
    ( NoOp, model ) ->
      ( model, Effects.none )

    ( StartOrPause, Initial ) ->
      ( Running startGame, Effects.none )

    ( StartOrPause, Running state ) ->
      ( Paused state, Effects.none )

    ( StartOrPause, Paused state ) ->
      ( Running state, Effects.none )

    ( StartOrPause, Over state ) ->
      ( Running startGame, Effects.none )

    ( Input c, Running state ) ->
      let
        input' =
          state.input ++ (String.fromChar c)
      in
        if toUpper input' == toUpper state.current.word then
          case state.left of
            [] ->
              ( Over { state | input = input' }, Effects.none )

            x :: xs ->
              ( Running { state | input = "", current = x, left = xs, done = state.current :: state.done }, Effects.none )
        else
          ( Running { state | input = input' }, Effects.none )

    ( Input c, model ) ->
      ( model, Effects.none )

    ( Backspace, Running state ) ->
      let
        input' =
          String.slice 0 -1 state.input
      in
        ( Running { state | input = input' }, Effects.none )

    ( Backspace, model ) ->
      ( model, Effects.none )


viewDiv : Address Action -> String -> String -> String -> Html
viewDiv address statusText inputValue statusClass =
  div
    []
    [ div [ class ("info " ++ statusClass) ] [ text statusText ]
    , input
        [ autofocus True
        , onWithOptions
            "keydown"
            { preventDefault = True, stopPropagation = True }
            keyCode
            (\s -> Signal.message address (keyboard s))
        , value inputValue
        ]
        []
    ]


view : Address Action -> Model -> Html
view address model =
  case model of
    Initial ->
      viewDiv address "Paina välilyöntiä aloittaaksesi" "" "initial"

    Running state ->
      viewDiv address state.current.number state.input "running"

    Paused state ->
      viewDiv address "Pysäytetty, paina välilyöntiä jatkaaksesi" state.input "paused"

    Over state ->
      viewDiv address "Peli on loppu, paina välilyöntiä aloittaaksesi uuden" state.input "over"


app : App Model
app =
  start
    { init = init
    , inputs = []
    , update = update
    , view = view
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
