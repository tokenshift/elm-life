module Settings exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Utils exposing (..)

type alias State = { rows : Int, cols : Int }

init : State
init = { rows = 16, cols = 16 }

type Msg
    = UpdateRows String
    | UpdateCols String
    | Create

type UpdateResult
    = NewState State
    | NewGrid Int Int

update : Msg -> State -> UpdateResult
update msg state =
    case msg of
        UpdateRows input ->
            NewState { state | rows = intOr state.rows input }
        UpdateCols input ->
            NewState { state | cols = intOr state.cols input }
        Create ->
            NewGrid state.rows state.cols

view : State -> Html Msg
view state =
    div [class "settings"]
        [ fieldset []
            [ label [] [ text "Rows" ]
            , input [type_ "number", value <| toString state.rows, onInput UpdateRows] []
            ]
        , fieldset []
            [ label [] [ text "Cols" ]
            , input [type_ "number", value <| toString state.cols, onInput UpdateCols] []
            ]
        , button [ onClick Create ] [ text "Create" ]
        ]

