module App.View.Menu exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import Style exposing (..)
import Style.Color as Color
import Style.Transition as Transition


-- MODEL


type State
    = Opened
    | Closed


type alias Model =
    State


init : Model
init =
    Closed



-- MESSAGES


type Msg
    = NoOp
    | SelectMenu



-- STYLE


type Styles
    = None
    | Backdrop
    | Container


type Variations
    = Hidden


styles : List (Style Styles Variations)
styles =
    [ Style.style None []
    , Style.style Backdrop
        [ Color.background black
        , Style.opacity 0.5
        , Style.prop "visibility" "visible"
        , Transition.transitions
            [ { delay = 0
              , duration = 300
              , easing = "ease"
              , props = [ "opacity", "visibility" ]
              }
            ]
        , Style.variation Hidden
            [ Style.opacity 0
            , Style.prop "visibility" "hidden"
            ]
        ]
    , Style.style Container
        [ Color.background white
        , Transition.transitions
            [ { delay = 0
              , duration = 200
              , easing = "ease-out"
              , props = [ "left" ]
              }
            ]
        ]
    ]



-- VIEW


view : { a | toMsg : Msg -> msg } -> Model -> Element Styles Variations msg
view { toMsg } model =
    column None
        []
        [ el Backdrop
            [ width fill
            , height fill
            , Events.onClick (toMsg SelectMenu)
            , vary Hidden
                (case model of
                    Opened ->
                        False

                    Closed ->
                        True
                )
            ]
            empty
            |> screen
        , el Container
            [ width (px 300)
            , height fill
            , moveLeft
                (case model of
                    Opened ->
                        0

                    Closed ->
                        300
                )
            ]
            empty
            |> screen
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectMenu ->
            case model of
                Opened ->
                    ( Closed, Cmd.none )

                Closed ->
                    ( Opened, Cmd.none )
