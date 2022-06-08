module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom as BrowserDom exposing (Element, Error)
import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Attribute, Html, a, div, h1, h2, h5, img, li, node, p, section, source, span, strong, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, id, media, rel, src, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Html.Events.Extra.Mouse as Mouse
import Layout exposing (initLayout)
import Page
import Request
import Round
import Shared
import Svg exposing (desc)
import Svg.Attributes exposing (transform)
import Task
import Utils.View exposing (customProp, materialIcon)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }



-- INIT


type alias Model =
    { mouseStart : { x : Float, y : Float }
    , startBgSize : { w : Float, h : Float }
    }


init : ( Model, Cmd Msg )
init =
    ( { mouseStart = { x = 0, y = 0 }
      , startBgSize = { w = 0, h = 0 }
      }
    , BrowserDom.getElement idStart
        |> Task.attempt GetStartBgSize
    )



-- UPDATE


type Msg
    = MouseStart ( Float, Float )
    | GetStartBgSize (Result Error Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseStart ( x_, y_ ) ->
            ( { model | mouseStart = { x = x_, y = y_ } }, Cmd.none )

        GetStartBgSize result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok e_ ->
                    ( { model
                        | startBgSize =
                            { w = e_.element.width
                            , h = e_.element.height
                            }
                      }
                    , Cmd.none
                    )



-- SUBS


subs : Model -> Sub Msg
subs model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , body =
        Layout.viewLayout
            { initLayout
                | route = Route.Home_
                , headerContent = viewHeader model
                , mainContent = viewPage model
            }
    }


viewHeader : Model -> List (Html Msg)
viewHeader model =
    [ a [ href "#" ] [ text "Explore" ]
    , materialIcon "person_pin_circle"
    , a [ href "#" ] [ text "Journal" ]
    , a [ href "#" ]
        [ materialIcon "search"
        , text "Search"
        ]
    ]


baseImageLink : String
baseImageLink =
    -- "https://gateway.pinata.cloud/ipfs/QmRtxKmBPYsANjbsmgXU55ZjNTh4fk2D3vvyd5B3NMorrV/bg@"
    "bg/bg@"


srcset : List String -> Attribute Msg
srcset url =
    String.join ", " url
        |> attribute "srcset"


viewPage : Model -> List (Html Msg)
viewPage model =
    [ viewStart model, viewIntro model ]


idStart : String
idStart =
    "start--id"


viewStart : Model -> Html Msg
viewStart model =
    let
        listMd : List Int
        listMd =
            [ 1536, 1280, 1024, 768 ]

        m : { x : Float, y : Float }
        m =
            { x = model.mouseStart.x, y = model.mouseStart.y }

        s : { w : Float, h : Float }
        s =
            { w = model.startBgSize.w, h = model.startBgSize.h }

        sHalf : { w : Float, h : Float }
        sHalf =
            { w = s.w / 2, h = s.h / 2 }

        mBasePosition : { x : Float, y : Float }
        mBasePosition =
            { x = m.x - sHalf.w, y = m.y - sHalf.h }

        normalizeTransform : { x : Float, y : Float }
        normalizeTransform =
            { x = mBasePosition.x / sHalf.w, y = mBasePosition.y / sHalf.h }
    in
    section
        [ class "start"
        , id idStart
        , ariaLabelledby "title--start"
        , Mouse.onMove (.clientPos >> MouseStart)
        ]
        [ node "picture"
            [ class "start__bg" ]
            (List.indexedMap
                (\i md ->
                    let
                        correctUrl =
                            String.fromInt >> String.padLeft 2 '0'

                        invertValue =
                            List.length listMd - i - 1
                    in
                    source
                        [ media <| "(min-width:" ++ String.fromInt md ++ "px)"
                        , srcset
                            [ baseImageLink ++ correctUrl invertValue ++ ".webp"
                            , baseImageLink ++ correctUrl invertValue ++ ".jpg"
                            ]
                        ]
                        []
                )
                listMd
                ++ [ img
                        [ src (baseImageLink ++ "00.webp")
                        , alt "background image"
                        , String.concat
                            [ "transform:scale(1.025) translate("
                            , Round.round 3 (normalizeTransform.x * -1)
                            , "rem,"
                            , Round.round 3 (normalizeTransform.y * -1)
                            , "rem);"
                            ]
                            |> attribute "style"
                        ]
                        []
                   ]
            )
        , div
            [ class "grid"
            , String.concat
                [ "transform:translate("
                , Round.round 3 (normalizeTransform.x / 3)
                , "rem,"
                , Round.round 3 (normalizeTransform.y / 3)
                , "rem);"
                ]
                |> attribute "style"
            ]
            [ h1 [ class "start__title", id "title--start" ] [ text "the great outdoors" ]
            , p [ class "start__text" ]
                [ --
                  text "Wander often. wonder always."

                --   text <| String.concat [ "x:", Round.round 3 normalizeTransform.x, " y:", Round.round 3 normalizeTransform.y ]
                ]
            ]
        ]


viewIntro : Model -> Html Msg
viewIntro _ =
    section [ class "intro", ariaLabelledby "title--intro" ]
        [ h2 [ class "intro__title", id "title--intro" ] [ text "Explore the World" ]
        , p [ class "intro__text" ]
            [ text """We seek to provide the most authentic content from athletes,
             adventurers, explorers and travellers around the world. Our long-term
             mission is to educate, inspire and enable all peoples to experience & 
             protect wilderness.""" ]
        , List.map
            (\{ url, place, country } ->
                li [ class "card" ]
                    [ img [ class "card__img", src url, alt <| "Photo " ++ place ] []
                    , div [ class "card__wrapper" ]
                        [ strong [ class "card__title" ] [ text place ]
                        , p [ class "card__text" ] [ text country ]
                        ]
                    ]
            )
            [ { url = "https://picsum.photos/800/1200"
              , place = "Nekajlskd"
              , country = "norway"
              }
            , { url = "https://picsum.photos/800/1250"
              , place = "Nekajlskd"
              , country = "norway"
              }
            , { url = "https://picsum.photos/800/1100"
              , place = "Nekajlskd"
              , country = "norway"
              }
            ]
            |> ul [ class "intro__list" ]
        , a [ class "intro__link", href "#" ]
            [ text "see more", materialIcon "navigate_next" ]
        ]
