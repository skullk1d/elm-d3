effect module App.Mouse
    where { subscription = MySub }
    exposing
        ( JSON
        , clicks
        , globalMouse
        , moves
        , downs
        , ups
        )

{-
   This is a modified version of the official Elm Mouse API:
     http://package.elm-lang.org/packages/elm-lang/mouse/latest/Mouse

   It differs from the original in two ways:

     (1) This module exposes the whole Json.Decode.Value object associated with
         a given mouse event, and not just the pageX/pageY properties.

     (2) As a result, you must build your own JSON decoder and run it on the
         mouse event JSONs when you use this module.

   Apart from replacing references to Position with references to Json.Decode.Value,
   the only change relative to the original the code is the replacement of "position"
   with Json.value at line 154 (line 179 of the original).

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)


-- MOUSE EVENTS


type alias JSON =
    Json.Value



{- Custom listener. -}


globalMouse : String -> (JSON -> msg) -> Sub msg
globalMouse eventType tagger =
    subscription (MySub eventType tagger)



{- Standard listeners. -}


clicks : (JSON -> msg) -> Sub msg
clicks tagger =
    subscription (MySub "click" tagger)


moves : (JSON -> msg) -> Sub msg
moves tagger =
    subscription (MySub "mousemove" tagger)


downs : (JSON -> msg) -> Sub msg
downs tagger =
    subscription (MySub "mousedown" tagger)


ups : (JSON -> msg) -> Sub msg
ups tagger =
    subscription (MySub "mouseup" tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (JSON -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (JSON -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (JSON -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp json maybeJsons =
    case maybeJsons of
        Nothing ->
            Just [ json ]

        Just jsons ->
            Just (json :: jsons)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , mouseInfo : JSON
    }


(&>) : Task x a -> Task x b -> Task x b
(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            task
                |> Task.andThen (\state -> Task.succeed (Dict.insert category (Watcher taggers pid) state))

        rightStep category taggers task =
            let
                tracker =
                    Dom.onDocument category Json.value (Platform.sendToSelf router << Msg category)
            in
                task
                    |> Task.andThen
                        (\state ->
                            Process.spawn tracker
                                |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Watcher taggers pid) state))
                        )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { category, mouseInfo } state =
    case Dict.get category state of
        Nothing ->
            Task.succeed state

        Just { taggers } ->
            let
                send tagger =
                    Platform.sendToApp router (tagger mouseInfo)
            in
                Task.sequence (List.map send taggers)
                    &> Task.succeed state
