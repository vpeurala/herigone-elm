module Main (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp exposing (App, start)
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
  , time : Int
  }


type Action
  = NoOp
  | Start


randomAssociation : Association
randomAssociation =
  { number = "00", word = "hiha" }


startGame : GameState
startGame =
  { done = []
  , left = allAssociations
  , current = randomAssociation
  , time = 0
  }


init : ( Model, Effects Action )
init =
  ( Initial
  , Effects.none
  )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    Start ->
      ( Running startGame, Effects.none )


view : Address Action -> Model -> Html
view address model =
  case model of
    Initial ->
      div
        [ onKeyDown
            address
            (\code ->
              case code of
                32 ->
                  Start

                _ ->
                  Start
            )
        ]
        [ text "Initial" ]

    Running state ->
      div [] [ text "Running" ]

    Paused state ->
      div [] [ text "Paused" ]

    Over state ->
      div [] [ text "Over" ]


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
