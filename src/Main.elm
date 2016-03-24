module Main (..) where

import Char exposing (KeyCode, fromCode, isLower, isUpper)
import Debug exposing (..)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (autofocus, tabindex, value)
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
  case (log "keyboard: " x) of
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
        if isUpper ch || isLower ch then
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

    ( StartOrPause, model ) ->
      case model of
        Initial ->
          ( Running startGame, Effects.none )

        Running state ->
          ( Paused state, Effects.none )

        Paused state ->
          ( Running state, Effects.none )

        Over state ->
          ( Running startGame, Effects.none )

    ( Input c, model ) ->
      case model of
        Initial ->
          ( Initial, Effects.none )

        Running state ->
          let
            input' =
              state.input ++ (String.fromChar c)
          in
            if toUpper input' == toUpper state.current.word then
              case state.left of
                [] ->
                  ( Over state, Effects.none )

                x :: xs ->
                  ( Running { state | input = "", current = x, left = xs }, Effects.none )
            else
              ( Running { state | input = input' }, Effects.none )

        Paused state ->
          ( Paused state, Effects.none )

        Over state ->
          ( Over state, Effects.none )

    ( Backspace, model ) ->
      case model of
        Initial ->
          ( Initial, Effects.none )

        Running state ->
          let
            input' =
              String.slice 0 -1 state.input
          in
            ( Running { state | input = input' }, Effects.none )

        Paused state ->
          ( Paused state, Effects.none )

        Over state ->
          ( Over state, Effects.none )


view : Address Action -> Model -> Html
view address model =
  case model of
    Initial ->
      div
        []
        [ input
            [ autofocus True
            , onWithOptions
                "keydown"
                { preventDefault = True, stopPropagation = True }
                keyCode
                (\s -> Signal.message address (keyboard s))
            ]
            []
        , div [] [ text "Initial" ]
        ]

    Running state ->
      div
        []
        [ input
            [ autofocus True
            , onWithOptions
                "keydown"
                { preventDefault = True, stopPropagation = True }
                keyCode
                (\s -> Signal.message address (keyboard s))
            , value state.input
            ]
            []
        , div [] [ text ("Running " ++ state.input) ]
        ]

    Paused state ->
      div
        []
        [ input
            [ autofocus True
            , onWithOptions
                "keydown"
                { preventDefault = True, stopPropagation = True }
                keyCode
                (\s -> Signal.message address (keyboard s))
            ]
            []
        , div [] [ text "Paused" ]
        ]

    Over state ->
      div
        []
        [ input
            [ autofocus True
            , onWithOptions
                "keydown"
                { preventDefault = True, stopPropagation = True }
                keyCode
                (\s -> Signal.message address (keyboard s))
            ]
            []
        , div [] [ text "Over" ]
        ]


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
