module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom as BrowserDom exposing (Element, Error)
import Components.Svg as SVG exposing (Logo(..))
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Attribute, Html, a, div, h1, h2, h3, h5, img, li, node, object, p, section, source, span, strong, text, ul)
import Html.Attributes exposing (alt, attribute, class, href, id, media, rel, src, tabindex, target)
import Html.Attributes.Aria exposing (ariaLabel, ariaLabelledby)
import Html.Events.Extra.Mouse as Mouse
import Layout exposing (headerId, initLayout)
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

    -- Size
    , startBgSize : { w : Float, h : Float }
    , headerSize : { h : Float, check : Bool }
    }


init : ( Model, Cmd Msg )
init =
    ( { mouseStart = { x = 0, y = 0 }

      -- Size
      , startBgSize = { w = 0, h = 0 }
      , headerSize = { h = 0, check = False }
      }
    , Cmd.batch
        [ BrowserDom.getElement idStart
            |> Task.attempt GetStartBgSize
        , BrowserDom.getElement headerId
            |> Task.attempt GetHeaderSize
        ]
    )



-- UPDATE


type Msg
    = MouseStart ( Float, Float )
    | GetStartBgSize (Result Error Element)
    | GetHeaderSize (Result Error Element)


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

        GetHeaderSize result ->
            case result of
                Err _ ->
                    ( { model
                        | headerSize =
                            { h = 0
                            , check = True
                            }
                      }
                    , Cmd.none
                    )

                Ok e_ ->
                    ( { model
                        | headerSize =
                            { h = e_.element.height
                            , check = False
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
                , rootAttrs =
                    [ if model.headerSize.check then
                        class ""

                      else
                        customProp ( "root-header-height", Round.round 0 model.headerSize.h ++ "px" )
                    ]
                , headerContent = viewHeader model
                , mainContent = viewPage model
                , footerAttrs = [ class "footer--home" ]
                , footerContent = viewFooter model
            }
    }


viewHeader : Model -> List (Html Msg)
viewHeader model =
    [ a [ href "#explore" ] [ text "Explore" ]
    , materialIcon "person_pin_circle"
    , a [ href "#title--intro" ] [ text "Journal" ]
    , a [ href "#journal" ]
        [ materialIcon "search"
        , text "Search"
        ]
    ]


baseImageLink : String
baseImageLink =
    -- "bg/bg@"
    "https://s8.gifyu.com/images/"


srcset : List String -> Attribute Msg
srcset url =
    String.join ", " url
        |> attribute "srcset"


viewPage : Model -> List (Html Msg)
viewPage model =
    [ viewStart model, viewIntro model, viewJornal model ]


idStart : String
idStart =
    "start--id"


viewStart : Model -> Html Msg
viewStart model =
    let
        listMd : List ( Int, String, String )
        listMd =
            List.map (\( x, y, z ) -> ( x, baseImageLink ++ y, baseImageLink ++ z ))
                [ ( 1536, "bg037c12a08e60239fa2", "bg037f7d77bc4b1a1471" )
                , ( 1280, "bg027c49899ea80f0d22", "bg02499ecb810f192257" )
                , ( 1024, "bg0152d57d4cb91bc109", "bg019e8c0216e6b0162b" )
                , ( 768, "bg0009959450f780a002", "bg001ee8ec8769ff9ca8" )
                ]

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
            (List.map
                (\( md, ulrWebp, ulrJpg ) ->
                    source
                        [ media <| "(min-width:" ++ String.fromInt md ++ "px)"
                        , srcset
                            [ ulrWebp ++ ".webp"
                            , ulrJpg ++ ".jpg"
                            ]
                        ]
                        []
                )
                listMd
                ++ [ img
                        [ src (baseImageLink ++ "bg001ee8ec8769ff9ca8.jpg")
                        , alt "background image"
                        , String.concat
                            [ "transform:scale(1.05) translate("
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
                , "rem); translateZ(0);"
                ]
                |> attribute "style"
            ]
            [ h1 [ class "start__title", id "title--start" ] [ text "the great outdoors" ]
            , p [ class "start__text" ] [ text "Wander often. wonder always." ]
            ]
        ]


viewIntro : Model -> Html Msg
viewIntro _ =
    section [ class "intro", id "explore", ariaLabelledby "title--intro" ]
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
            [ { url = "https://s8.gifyu.com/images/tim-stief-YFFGkE3y4F8-unsplash.webp"
              , place = "Nærøyfjorden"
              , country = "norway"
              }
            , { url = "https://s8.gifyu.com/images/dani-garcia-fJQamCZIZf8-unsplash.webp"
              , place = "Antelope Canyon"
              , country = "United states"
              }
            , { url = "https://s8.gifyu.com/images/karsten-wurth-FYMvmVz3OQo-unsplash.webp"
              , place = "Grossglockner"
              , country = "Austria"
              }
            ]
            |> ul [ class "intro__list" ]
        , a [ class "intro__link", href "#" ]
            [ text "see more", materialIcon "navigate_next" ]
        ]


viewJornal : Model -> Html Msg
viewJornal _ =
    section [ class "journal", id "journal", ariaLabelledby "title--jornal" ]
        [ h3 [ class "journal__title", id "title--jornal" ] [ text "Journal" ]
        , p [ class "journal__text" ]
            [ text """Our favorite stories about public lands and opportunities 
            for you to get involved in protecting your outdoor experiences.""" ]
        , ul [ class "journal__list" ]
            [ viewUpdates
                { ulr = "https://s8.gifyu.com/images/madhu-shesharam-HRA_VAi9_Nc-unsplash.webp"
                , date = "May 28, 2017"
                , title = "An Unforgettable"
                , desc = """If you only have one day to visit Yosemite
                 National Park and you want to make the most out of it."""
                }
            , viewUpdates
                { ulr = "https://s8.gifyu.com/images/joseph-barrientos-Ji_G7Bu1MoM-unsplash.webp"
                , date = "May 30, 2017"
                , title = "Symphonies in Steel"
                , desc = """Crossing the Golden Gate Bridge from San 
                 Francisco, you arrive in Marin even before landing 
                 on solid ground."""
                }
            ]
        , a [ class "journal__link", href "#" ]
            [ text "All Post", materialIcon "navigate_next" ]
        ]


viewUpdates : { ulr : String, date : String, title : String, desc : String } -> Html Msg
viewUpdates data =
    li [ class "news" ]
        [ img [ class "news__img", src data.ulr ] []
        , p [ class "news__date" ] [ text data.date ]
        , p [ class "news__title" ] [ text data.title ]
        , p [ class "news__text" ] [ text data.desc ]
        ]


viewFooter : Model -> List (Html Msg)
viewFooter _ =
    [ img [ class "img", src "https://s8.gifyu.com/images/shot-by-cerqueira-XTKjhfE-Inc-unsplash.webp" ] []
    , div [ class "wrapper" ]
        [ p []
            [ text "© 2022 "
            , span [ class "uppercase" ] [ text "The great outdoors." ]
            , text " All rights reserved."
            ]
        , a [ class "uppercase", href "#" ] [ text "About" ]
        , a [ class "uppercase", href "#explore" ] [ text "Explore" ]
        , a [ class "uppercase", href "#" ] [ text "Journal" ]
        , a [ class "uppercase", href "#" ] [ text "Search" ]
        ]
    ]
