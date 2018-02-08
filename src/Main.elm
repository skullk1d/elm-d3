module Main exposing (..)

{-| This is the main module
@docs main
-}

import Navigation
import App.State exposing (..)
import App.View exposing (..)


{-| This is the main function
-}
main : Program Flags Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
