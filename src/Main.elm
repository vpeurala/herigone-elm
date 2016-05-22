module Main (..) where

import Char exposing (KeyCode, fromCode, isLower, isUpper)
import Debug exposing (..)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, tabindex, value)
import Html.Events exposing (..)
import Random exposing (initialSeed, int, generate, list)
import Signal exposing (Address)
import String exposing (fromChar, toUpper)
import Task exposing (Task)
import Time exposing (Time)
import TimeApp exposing (App, start)
import Associations exposing (..)


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
  , timer : Float
  }


type Action
  = NoOp
  | StartOrPause
  | Input Char
  | Backspace
  | Tick


shuffle : List a -> Int -> List a
shuffle xs seed =
  let
    gen =
      list (List.length xs) (int 0 1000)

    seed' =
      initialSeed seed

    ( rands, _ ) =
      generate gen seed'

    zips =
      List.map2 (,) xs rands

    sorted =
      List.sortBy snd zips
  in
    List.map fst sorted


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
  case (shuffle allAssociations 1178) of
    [] ->
      Debug.crash "No associations!"

    x :: xs ->
      { done = []
      , left = xs
      , current = x
      , input = ""
      , timer = 0
      }


init : ( Model, Effects Action )
init =
  ( Initial
  , Effects.none
  )


noFx : Model -> ( Model, Effects Action )
noFx m =
  ( m, Effects.none )


update : Action -> Time -> Model -> ( Model, Effects Action )
update action time model =
  noFx
    (case ( action, model ) of
      ( NoOp, model ) ->
        model

      ( StartOrPause, Initial ) ->
        Running startGame

      ( StartOrPause, Running state ) ->
        Paused state

      ( StartOrPause, Paused state ) ->
        Running state

      ( StartOrPause, Over state ) ->
        Running startGame

      ( Input c, Running state ) ->
        let
          input' =
            state.input ++ (String.fromChar c)
        in
          if toUpper input' == toUpper state.current.word then
            case state.left of
              [] ->
                Over { state | input = input' }

              x :: xs ->
                Running { state | input = "", current = x, left = xs, done = state.current :: state.done }
          else
            Running { state | input = input' }

      ( Input c, model ) ->
        model

      ( Backspace, Running state ) ->
        let
          input' =
            String.slice 0 -1 state.input
        in
          Running { state | input = input' }

      ( Backspace, model ) ->
        model

      ( Tick, Running state ) ->
        Running { state | timer = state.timer + 1 }

      ( Tick, model ) ->
        model
    )


viewRunning : Address Action -> GameState -> Html
viewRunning address state =
  div
    []
    [ div [ class ("info running") ] [ text state.current.number ]
    , input
        [ autofocus True
        , onWithOptions
            "keydown"
            { preventDefault = True, stopPropagation = True }
            keyCode
            (\s -> Signal.message address (keyboard s))
        , value state.input
        ]
        []
    , div [] [ text (toString state.timer) ]
    ]


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
      viewRunning address state

    Paused state ->
      viewDiv address "Pysäytetty, paina välilyöntiä jatkaaksesi" state.input "paused"

    Over state ->
      viewDiv address "Peli on loppu, paina välilyöntiä aloittaaksesi uuden" state.input "over"


const : b -> (a -> b)
const x =
  \_ -> x


app : App Model
app =
  start
    { init = init
    , inputs = [ Signal.map (const Tick) (Time.fps 24) ]
    , update = update
    , view = view
    }


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
