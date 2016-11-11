module Main exposing (..)

import Char exposing (KeyCode, fromCode, isLower, isUpper)
import Json.Decode exposing (Decoder)
import Json.Decode as D
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Generator(..), initialSeed, int, generate, list)
import String exposing (fromChar, toUpper)
import Time exposing (Time)
import Time


-- OS

import Formatting exposing (..)
import Keyboard
import List.Nonempty exposing (Nonempty)
import List.Nonempty as Nonempty


-- Application

import Associations exposing (..)
import NonemptyUtil exposing (shuffle)


-- Code


type Model
    = Initial
    | Running GameState
    | Paused GameState
    | Over GameState


type alias GameState =
    { done : List AssociationWithTime
    , left : List Association
    , current : Association
    , input : String
    , timer : Timer
    }


type alias AssociationWithTime =
    { association : Association
    , duration : Time
    }


type alias Timer =
    { currentTime : Time
    , timeAtStartOfThisAssociation : Time
    }


type Msg
    = NoOp
    | StartOrPause
    | Input Char
    | Backspace
    | Tick Time
    | InitialState Model


randomModel : Random.Generator Model
randomModel =
    Random.map generateInitialModelFromRandomListOfInts nonEmptyRandomListOfInts


getInitialState : Cmd Msg
getInitialState =
    Random.generate (\ns -> InitialState (generateInitialModelFromRandomListOfInts ns)) nonEmptyRandomListOfInts


generateInitialModelFromRandomListOfInts : Model
generateInitialModelFromRandomListOfInts =
    let
        allAssociationsInRandomOrder =
            shuffle allAssociations
    in
        Running
            { current = Nonempty.head allAssociationsInRandomOrder
            , done = []
            , input = ""
            , left = Nonempty.tail allAssociationsInRandomOrder
            , timer =
                { currentTime = 0
                , timeAtStartOfThisAssociation = 0
                }
            }


keyboard : Int -> Msg
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


init : ( Model, Cmd Msg )
init =
    ( Initial
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    (case ( action, model ) of
        ( NoOp, model ) ->
            ( model, Cmd.none )

        ( StartOrPause, Initial ) ->
            ( Initial, getInitialState )

        ( StartOrPause, Running state ) ->
            ( Paused state, Cmd.none )

        ( StartOrPause, Paused state ) ->
            ( Running state, Cmd.none )

        ( StartOrPause, Over state ) ->
            ( Initial, getInitialState )

        ( Input c, Running state ) ->
            let
                input' =
                    state.input ++ (String.fromChar c)
            in
                if toUpper input' == toUpper state.current.word then
                    case state.left of
                        [] ->
                            ( Over { state | input = input' }, Cmd.none )

                        x :: xs ->
                            ( Running
                                { state
                                    | input = ""
                                    , current = x
                                    , left = xs
                                    , done =
                                        { association = state.current
                                        , duration = (state.timer.currentTime - state.timer.timeAtStartOfThisAssociation)
                                        }
                                            :: state.done
                                    , timer =
                                        { currentTime = state.timer.currentTime
                                        , timeAtStartOfThisAssociation = state.timer.currentTime
                                        }
                                }
                            , Cmd.none
                            )
                else
                    ( Running { state | input = input' }, Cmd.none )

        ( Input c, model ) ->
            ( model, Cmd.none )

        ( Backspace, Running state ) ->
            let
                input' =
                    String.slice 0 -1 state.input
            in
                ( Running { state | input = input' }, Cmd.none )

        ( Backspace, model ) ->
            ( model, Cmd.none )

        ( Tick time, Running state ) ->
            let
                timer =
                    state.timer

                timer' =
                    { timer | currentTime = timer.currentTime + 1 }
            in
                ( Running { state | timer = timer' }, Cmd.none )

        ( Tick time, model ) ->
            ( model, Cmd.none )

        ( InitialState newModel, model ) ->
            ( newModel, Cmd.none )
    )


viewRunning : GameState -> Html Msg
viewRunning state =
    div [ class "game" ]
        [ div [ class ("info running") ] [ text state.current.number ]
        , input
            [ autofocus True
            , onWithOptions "keydown"
                { preventDefault = True, stopPropagation = True }
                decodeKeyCode
            , value state.input
            ]
            []
          {--
        , case state.done of
            [] ->
                div [ style [ ( "display", "none" ) ] ] []

            x :: xs ->
                div
                    [ id x.association.word
                    , class
                        ("animated "
                            ++ x.association.word
                        )
                    ]
                    [ text (x.association.number ++ x.association.word ++ (toString x.duration)) ]
                    --}
        , div [ class "timer" ] [ text (formatWholeGameTimer state) ]
        , div [ class "timer" ] [ text (formatCurrentWordTimer state) ]
        , div [ class "associations-left" ] [ text (toString (List.length state.left) ++ " jäljellä") ]
        , div [ class "debug" ] [ text (toString state) ]
        ]


formatWholeGameTimer : GameState -> String
formatWholeGameTimer state =
    print (padLeft 6 '0' (Formatting.roundTo 2)) (state.timer.currentTime / 25) ++ "s"


formatCurrentWordTimer : GameState -> String
formatCurrentWordTimer state =
    print (padLeft 6 '0' (Formatting.roundTo 2)) ((state.timer.currentTime - state.timer.timeAtStartOfThisAssociation) / 25) ++ "s"


viewDiv : GameState -> String -> String -> String -> Html Msg
viewDiv state statusText inputValue statusClass =
    div [ class "game" ]
        [ div [ class ("info " ++ statusClass) ] [ text statusText ]
        , input
            [ autofocus True
            , onWithOptions "keydown"
                { preventDefault = True, stopPropagation = True }
                decodeKeyCode
            , value inputValue
            ]
            []
        , div [ class "timer" ] [ text (formatWholeGameTimer state) ]
        , div [ class "timer" ] [ text (formatCurrentWordTimer state) ]
        , div [ class "associations-left" ] [ text (toString (List.length state.left) ++ " jäljellä") ]
        , div [ class "debug" ] [ text (toString state) ]
        ]


viewInitial : Html Msg
viewInitial =
    div [ class "game" ]
        [ div [ class "info initial" ] [ text "Paina välilyöntiä aloittaaksesi" ]
        , input
            [ autofocus True
            , onWithOptions "keydown"
                { preventDefault = True, stopPropagation = True }
                decodeKeyCode
            , value ""
            ]
            []
        ]


decodeKeyCode : Decoder Msg
decodeKeyCode =
    D.int `D.andThen` (\i -> D.succeed (keyboard i))


view : Model -> Html Msg
view model =
    (case model of
        Initial ->
            viewInitial

        Running state ->
            viewRunning state

        Paused state ->
            viewDiv state "Pysäytetty, paina välilyöntiä jatkaaksesi" state.input "paused"

        Over state ->
            viewDiv state "Peli on loppu, paina välilyöntiä aloittaaksesi uuden" state.input "over"
    )


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (\kc -> (keyboard kc))
        , Time.every (40 * Time.millisecond) Tick
        ]
