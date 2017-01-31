module Main exposing (..)

import Html exposing (..)

import App exposing (init, update, view)

main = Html.beginnerProgram { model = App.init, update = App.update, view = App.view }
