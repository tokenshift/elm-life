module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (second, millisecond)

import Grid exposing (init)
import Settings exposing (init, update, view)

type Model
    = Setup Settings.State
    | Ready Grid.Model

init : (Model, Cmd Msg)
init = (Setup Settings.init, Cmd.none)

type Msg
    = SetupMsg Settings.Msg
    | ReadyMsg Grid.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg state =
    case state of
        Setup state ->
            (updateSetup msg state, Cmd.none)
        Ready state ->
            (updateReady msg state, Cmd.none)

updateSetup : Msg -> Settings.State -> Model
updateSetup msg state =
    case msg of
        SetupMsg msg ->
            case Settings.update msg state of
                Settings.NewState state ->
                    Setup state
                Settings.NewGrid rows cols ->
                    Ready <| Grid.init rows cols
        _ -> Setup state

updateReady : Msg -> Grid.Model -> Model
updateReady msg model =
    case msg of
        ReadyMsg msg ->
            Ready <| Grid.update msg model
        _ ->
            Ready model

view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ case model of
              Setup state ->
                  Html.map (\msg -> SetupMsg msg) <| Settings.view state
              Ready state ->
                  Html.map (\msg -> ReadyMsg msg) <| Grid.view state
        , pre [ class "debug" ] [ model |> toString |> text ]
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * millisecond) (\_ -> ReadyMsg Grid.MaybeIterate)
