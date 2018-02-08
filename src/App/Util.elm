module App.Util exposing (onPreventDefaultClick, onStopPropagationClick)

import Element exposing (Attribute)
import Element.Events exposing (onWithOptions, defaultOptions)
import Json.Decode exposing (succeed)


-- UTIL


onPreventDefaultClick : msg -> Attribute variation msg
onPreventDefaultClick message =
    onWithOptions
        "click"
        { defaultOptions | preventDefault = True }
        (Json.Decode.succeed message)


onStopPropagationClick : msg -> Attribute variation msg
onStopPropagationClick message =
    onWithOptions
        "click"
        { defaultOptions | stopPropagation = True }
        (Json.Decode.succeed message)
