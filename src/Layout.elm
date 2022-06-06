module Layout exposing (Model, initLayout, viewLayout)

import Gen.Route as Route exposing (Route)
import Html exposing (Attribute, Html, a, div, header, main_, nav, text)
import Html.Attributes exposing (class, classList, href, id, tabindex)
import Regex



-- Model


type alias Model msg =
    { route : Route
    , mainContent : List (Html msg)
    , mainAttrs : List (Attribute msg)
    , headerContent : List (Html msg)
    }


type alias Link =
    { routeStatic : Route
    , routeReceived : Route
    , routeName : String
    , hasMarginLeft : Bool
    }


initLayout : Model msg
initLayout =
    { route = Route.Home_
    , mainContent = []
    , mainAttrs = []
    , headerContent = []
    }


defaultLink : Link
defaultLink =
    { routeStatic = Route.Home_
    , routeReceived = Route.Home_
    , routeName = ""
    , hasMarginLeft = False
    }



-- Structure


isRoute : Route -> Route -> Bool
isRoute route compare =
    case ( route, compare ) of
        ( Route.Home_, Route.Home_ ) ->
            True

        _ ->
            False


routeName : Route -> String
routeName route =
    let
        strRoute : String
        strRoute =
            Route.toHref route

        getLength =
            String.length strRoute

        getFirstCharacter =
            String.dropRight (getLength - 2) strRoute
                |> String.toUpper
                |> String.dropLeft 1
    in
    if route == Route.Home_ then
        "Home"

    else
        getFirstCharacter
            ++ String.replace "/" " - " (String.dropLeft 2 strRoute)


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


classBuilder : String -> String
classBuilder string =
    userReplace "[ ]" (\_ -> "-") string
        |> String.toLower



-- View


viewLayout : Model msg -> List (Html msg)
viewLayout model =
    let
        mainClass : Attribute msg
        mainClass =
            class <| "main--" ++ classBuilder (routeName model.route)
    in
    [ div
        [ id "root"
        , classList [ ( "scroll", True ), ( "root--" ++ classBuilder (routeName model.route), True ) ]
        ]
        [ viewHeader model
        , main_ (mainClass :: model.mainAttrs) model.mainContent
        ]
    ]


viewHeader : Model msg -> Html msg
viewHeader model =
    header [ class "main-header" ] <|
        nav [ class "main-header__nav" ]
            (viewHeaderLinks model [ Route.Home_ ])
            :: model.headerContent


viewHeaderLinks : Model msg -> List Route -> List (Html msg)
viewHeaderLinks model links =
    List.map
        (\staticRoute ->
            viewLink
                { defaultLink
                    | routeName = routeName staticRoute
                    , routeStatic = staticRoute
                    , routeReceived = model.route
                }
        )
        links


viewLink : Link -> Html msg
viewLink model =
    a
        [ class "main-header__links"
        , classList
            [ ( "main-header__links--current-page"
              , isRoute model.routeReceived model.routeStatic
              )
            ]
        , href <| Route.toHref model.routeStatic
        , tabindex 1
        ]
        [ text model.routeName ]
