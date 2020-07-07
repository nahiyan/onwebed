module Main exposing (main)

import Browser exposing (element)
import Core exposing (FlagType, Model, Msg)
import State exposing (initialize, subscriptions, update)
import View exposing (view)


main : Program FlagType Model Msg
main =
    Browser.element
        { init = initialize
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
