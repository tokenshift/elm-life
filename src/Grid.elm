module Grid exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (..)

type CellState = Alive | Dead
type alias Point2 = (Int, Int)

type alias Model = { rows : Int, cols : Int, living : Set Point2, running : Bool }

init : Int -> Int -> Model
init rows cols =
    { rows = rows
    , cols = cols
    , living = Set.empty
    , running = False }

state : Point2 -> Model -> CellState
state (x, y) model =
    let
        x1 = x % model.cols
        y1 = y % model.cols
    in
       if Set.member (x1, y1) model.living
          then Alive
          else Dead

toggle : Point2 -> Model -> Model
toggle xy model =
    case state xy model of
        Alive ->
            { model | living = Set.remove xy model.living }
        Dead ->
            { model | living = Set.insert xy model.living }

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
        xs = List.range 0 (model.rows - 1)
        ys = List.range 0 (model.cols - 1)
        coords = List.concatMap (\x -> List.map (\y -> (x,y)) ys) xs

        newState1 = \xy -> newState (state xy model) (neighbors xy model)
        living = coords |> List.filter (\xy -> newState1 xy == Alive)
    in
        { model | living = Set.fromList living }

type Msg
    = ToggleCell Point2
    | Iterate
    | MaybeIterate
    | Reset
    | Play
    | Pause

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleCell xy -> toggle xy model
        Iterate -> iterate model
        MaybeIterate -> if model.running then iterate model else model
        Reset -> { model | living = Set.empty, running = False }
        Play -> { model | running = True }
        Pause -> { model | running = False }

view : Model -> Html Msg
view model =
    div [ class "grid" ]
        [ table [] (List.map (viewRow model) <| List.range 0 (model.rows - 1))
        , button [ onClick Iterate ] [ text "Iterate" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , if model.running
             then (button [ onClick Pause ] [ text "Pause" ])
             else (button [ onClick Play ] [ text "Play" ])
        ]

viewRow : Model -> Int -> Html Msg
viewRow model row =
    tr [] (List.map (viewCell model row) <| List.range 0 (model.cols - 1))

viewCell : Model -> Int -> Int -> Html Msg
viewCell model row col =
    let cell = state (row, col) model
    in td [ cell |> toString |> String.toLower |> class, onClick <| ToggleCell (row,col) ] []

