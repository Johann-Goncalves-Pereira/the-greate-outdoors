module Pages.Home_ exposing (Model, Msg, page)

import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Attribute, Html, a, div, h1, h2, h5, img, li, node, p, section, source, span, strong, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, id, media, rel, src, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Layout exposing (initLayout)
import Page
import Request
import Shared
import Svg exposing (desc)
import Utils.View exposing (customProp, materialIcon)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    {}


init : Model
init =
    {}



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReplaceMe ->
            model



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


viewStart : Model -> Html Msg
viewStart _ =
    let
        listMd =
            [ 1536, 1280, 1024, 768 ]
    in
    section [ class "start", ariaLabelledby "title--start" ]
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
                ++ [ img [ src (baseImageLink ++ "00.webp"), alt "background image" ] [] ]
            )
        , div [ class "grid" ]
            [ h1 [ class "start__title", id "title--start" ] [ text "the great outdoors" ]
            , p [ class "start__text" ] [ text "Wander often. wonder always." ]
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
