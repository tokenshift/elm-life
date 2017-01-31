module Grid exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type CellState = Alive | Dead

type alias Point2 = (Int, Int)

type alias Model = { rows : Int, cols : Int, cells : Dict Point2 CellState }

init : Int -> Int -> Model
init rows cols =
    { rows = rows
    , cols = cols
    , cells = Dict.empty }

state : Point2 -> Model -> CellState
state xy model =
    case Dict.get xy model.cells of
        Just val -> val
        Nothing -> Dead

toggle1 : Maybe CellState -> Maybe CellState
toggle1 cell =
    case cell of
        Nothing -> Just Alive
        Just Alive -> Nothing
        Just Dead -> Just Alive

toggle : Point2 -> Model -> Model
toggle xy model =
    { model | cells = Dict.update xy toggle1 model.cells }

neighbors : Point2 -> Model -> Int
neighbors (row, col) model =
    let
        liveness = \xy -> if state xy model == Alive then 1 else 0
        coords = [ (row - 1, col - 1)
                 , (row - 1, col)
                 , (row - 1, col + 1)
                 , (row, col - 1)
                 --, (row, col)
                 , (row, col + 1)
                 , (row + 1, col - 1)
                 , (row + 1, col)
                 , (row + 1, col + 1)
                 ]
    in
        List.foldl (+) 0 <| List.map liveness <| coords

newState : CellState -> Int -> CellState
newState oldState neighbors =
    case oldState of
        Alive ->
            if neighbors < 2 || neighbors > 3 then Dead else Alive
        Dead ->
            if neighbors == 3 then Alive else Dead

iterate : Model -> Model
iterate model =
    let
        xs = List.range 1 model.rows
        ys = List.range 1 model.cols
        coords = List.concatMap (\x -> List.map (\y -> (x,y)) ys) xs

        newState1 = \xy -> newState (state xy model) (neighbors xy model)
        insertNewState = \xy cells -> Dict.insert xy (newState1 xy) cells
        cells = coords |> List.foldl insertNewState Dict.empty
    in
        { model | cells = cells }

reset model =
    { model | cells = Dict.empty }

type Msg
    = ToggleCell Point2
    | Iterate
    | Reset

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleCell xy -> toggle xy model
        Iterate -> iterate model
        Reset -> reset model

view : Model -> Html Msg
view model =
    div [ class "grid" ]
        [ table [] (List.map (viewRow model) <| List.range 1 model.rows)
        , button [ onClick Iterate ] [ text "Iterate" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]

viewRow : Model -> Int -> Html Msg
viewRow model row =
    tr [] (List.map (viewCell model row) <| List.range 1 model.cols)

viewCell : Model -> Int -> Int -> Html Msg
viewCell model row col =
    let cell = state (row, col) model
    in td [ cell |> toString |> String.toLower |> class, onClick <| ToggleCell (row,col) ] []
