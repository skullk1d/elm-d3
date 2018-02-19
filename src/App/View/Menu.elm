module App.View.Menu exposing (..)

-- import Html exposing (..)
-- import Html.Attributes exposing (class)

import Navigation exposing (Location)
import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import App.Util exposing (onPreventDefaultClick)
import App.Routing as Routing exposing (Route(..))


type Msg
    = NoOp
    | NewRoute Route



-- STYLE


type Styles
    = None
    | Link


type Variations
    = Selected



-- Model


type alias Model =
    { selected : Route
    }


styles : List (Style Styles Variations)
styles =
    [ style None []
    , style Link
        [ Color.text darkRed
        , Font.underline
        , hover
            [ Color.text lightRed
            ]
        ]
    ]



-- INIT


init : Model
init =
    { selected = AboutRoute
    }



-- VIEW


view : Model -> Element Styles Variations Msg
view model =
    column
        None
        []
        [ link (Routing.routeToUrl AboutRoute) <|
            button Link
                [ onPreventDefaultClick <| NewRoute AboutRoute
                , vary Selected (model.selected == AboutRoute)
                ]
                (text "About")
        , link (Routing.routeToUrl TopRoute) <|
            button Link
                [ onPreventDefaultClick <| NewRoute TopRoute
                , vary Selected (model.selected == TopRoute)
                ]
                (text "Data")
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewRoute route ->
            ( { model | selected = route }, Navigation.newUrl (Routing.routeToUrl route) )
