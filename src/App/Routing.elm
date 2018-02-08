module App.Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


-- ROUTING


type Route
    = TopRoute
    | AboutRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map TopRoute top
        , map AboutRoute (s (routeToPath AboutRoute))
        ]



-- PUBLIC HELPERS


parseLocation : Location -> Route
parseLocation location =
    case (parsePath matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


routeToPath : Route -> String
routeToPath route =
    case route of
        TopRoute ->
            ""

        AboutRoute ->
            "about"

        NotFoundRoute ->
            "not-found"


routeToUrl : Route -> String
routeToUrl route =
    "/" ++ (routeToPath route)
