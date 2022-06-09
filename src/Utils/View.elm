module Utils.View exposing (..)

import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (attribute, class)


customProps : List { prop : String, value : String } -> Attribute msg
customProps listProps =
    List.foldl
        (\{ prop, value } result ->
            String.concat [ result, "--", prop, ":", value, ";" ]
        )
        ""
        listProps
        |> attribute "style"


customProp : ( String, String ) -> Attribute msg
customProp ( p, v ) =
    String.concat [ "--", p, ":", v, ";" ]
        |> attribute "style"


materialIcon : String -> Html msg
materialIcon icon =
    span [ class "material-symbols-outlined select-none" ] [ text icon ]
