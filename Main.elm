module Main exposing (..)

import Html exposing (..)

import App exposing (init, update, view)

main = Html.program { init = App.init, update = App.update, view = App.view, subscriptions = App.subscriptions }
