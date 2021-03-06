module App.State exposing (..)

import Navigation exposing (Location)
import App.Routing as Routing exposing (Route(..))
import App.Page.Top as Top
import App.Page.About as About
import App.View.Menu as Menu
import App.Port as Port


type Msg
    = NoOp
    | OnLocationChange Location
    | TopMsg Top.Msg
    | AboutMsg About.Msg
    | MenuMsg Menu.Msg
    | PortMsg Port.Msg


type alias Flags =
    { api : String }


type alias Model =
    { api : String
    , message : String
    , currentRoute : Route
    , top : Top.Model
    , ports : Port.Model
    }



-- include any initCmds for each Page that has them


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        topModel =
            Top.init

        ( portModel, portCmd ) =
            Port.init flags.api

        initModel =
            { api = flags.api
            , message = "D3 is lit"
            , currentRoute = Routing.parseLocation location
            , top = topModel
            , ports = portModel
            }

        ( model, routeCmd ) =
            update (OnLocationChange location) initModel
    in
        model
            ! [ routeCmd
              , Cmd.map PortMsg portCmd
              ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map PortMsg (Port.subscriptions model.ports)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        OnLocationChange location ->
            let
                newRoute =
                    Routing.parseLocation location
            in
                getRouteCmds newRoute model

        TopMsg msg_ ->
            -- TODO implement a helper function to map outgoing messages
            let
                ( topModel, topCmds ) =
                    Top.update msg_ model.top

                ( _, portCmds ) =
                    case msg_ of
                        Top.SubmitForm pForm ->
                            Port.update (Port.SubmitForm pForm) model.ports

                        _ ->
                            ( model.ports, Cmd.none )
            in
                { model | top = topModel } ! [ Cmd.map TopMsg topCmds, Cmd.map PortMsg portCmds ]

        AboutMsg msg_ ->
            let
                cmds =
                    About.update msg_
            in
                ( model, Cmd.map AboutMsg cmds )

        MenuMsg msg_ ->
            let
                cmds =
                    Menu.update msg_
            in
                ( model, Cmd.map MenuMsg cmds )

        PortMsg msg_ ->
            let
                ( model_, cmds ) =
                    Port.update msg_ model.ports

                topModel =
                    model.top

                newTopModel =
                    { topModel
                        | partitions = model_.partitions
                        , partitionWidth = model_.partitionParams.width
                        , partitionHeight = model_.partitionParams.height
                    }
            in
                ( { model | ports = model_, top = newTopModel }, Cmd.map PortMsg cmds )


getRouteCmds : Route -> Model -> ( Model, Cmd Msg )
getRouteCmds newRoute model =
    case newRoute of
        TopRoute ->
            let
                ( topModel, topCmd ) =
                    Top.onNavigate model.top model.api
            in
                ( { model | top = topModel, currentRoute = newRoute }, Cmd.map TopMsg topCmd )

        _ ->
            ( { model | currentRoute = newRoute }, Cmd.none )



-- include onNavigate for any Page that uses it
