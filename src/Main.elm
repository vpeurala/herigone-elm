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


-- import Task exposing (Task)
-- import Task as Task
-- import Time exposing (Time)
-- OS
-- import Basics.Extra exposing (never)

import Keyboard
import List.Nonempty exposing (Nonempty)
import List.Nonempty as Nonempty


-- Application

import Associations exposing (..)


-- Code


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


type Msg
    = NoOp
    | StartOrPause
    | Input Char
    | Backspace
    | Tick
    | InitialState Model



{--
shuffle : List a -> Int -> List a
shuffle xs seed =
    let
        gen =
            Random.list (List.length xs) (int 0 1000)

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
--}


getInitialState : Cmd Msg
getInitialState =
    generate (\is -> InitialState is)
        (Random.map generateInitialModelFromRandomListOfInts
            (case
                (Random.map Nonempty.fromList (Random.list (Nonempty.length allAssociations) (int 0 1000)))
             of
                _ ->
                    Debug.crash "This is impossible."
            )
        )


generateInitialModelFromRandomListOfInts : Nonempty Int -> Model
generateInitialModelFromRandomListOfInts rands =
    Debug.crash "generateInitialModelFromRandomListOfInts"


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


startGame : GameState
startGame =
    case (shuffle (Nonempty.toList allAssociations) 1178) of
        [] ->
            Debug.crash "No associations!"

        x :: xs ->
            { done = []
            , left = xs
            , current = x
            , input = ""
            , timer = 0
            }


shuffle : List Association -> Int -> List Association
shuffle list int =
    list


init : ( Model, Cmd Msg )
init =
    ( Initial
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( action, model ) of
        ( NoOp, model ) ->
            ( model, Cmd.none )

        ( StartOrPause, Initial ) ->
            ( Running startGame, getInitialState )

        ( StartOrPause, Running state ) ->
            ( Paused state, Cmd.none )

        ( StartOrPause, Paused state ) ->
            ( Running state, Cmd.none )

        ( StartOrPause, Over state ) ->
            ( Running startGame, getInitialState )

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
                                    , done = state.current :: state.done
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

        ( Tick, Running state ) ->
            ( Running { state | timer = state.timer + 1 }, Cmd.none )

        ( Tick, model ) ->
            ( model, Cmd.none )

        ( InitialState _, model ) ->
            ( model, Cmd.none )


viewRunning : GameState -> Html Msg
viewRunning state =
    div []
        [ div [ class ("info running") ] [ text state.current.number ]
        , input
            [ autofocus True
            , onWithOptions "keydown"
                { preventDefault = True, stopPropagation = True }
                decodeKeyCode
            , value state.input
            ]
            []
        , div [] [ text (toString state.timer) ]
        ]


viewDiv : String -> String -> String -> Html Msg
viewDiv statusText inputValue statusClass =
    div []
        [ div [ class ("info " ++ statusClass) ] [ text statusText ]
        , input
            [ autofocus True
            , onWithOptions "keydown"
                { preventDefault = True, stopPropagation = True }
                decodeKeyCode
            , value inputValue
            ]
            []
        ]


decodeKeyCode : Decoder Msg
decodeKeyCode =
    D.int `D.andThen` (\i -> D.succeed (keyboard i))


view : Model -> Html Msg
view model =
    Debug.log ("view, model: " ++ (toString model) ++ "")
        (case model of
            Initial ->
                viewDiv "Paina välilyöntiä aloittaaksesi" "" "initial"

            Running state ->
                viewRunning state

            Paused state ->
                viewDiv "Pysäytetty, paina välilyöntiä jatkaaksesi" state.input "paused"

            Over state ->
                viewDiv "Peli on loppu, paina välilyöntiä aloittaaksesi uuden" state.input "over"
        )


const : b -> (a -> b)
const x =
    \_ -> x


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
    Keyboard.ups (\kc -> (keyboard kc))
