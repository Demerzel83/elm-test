module Widgets.Dropdown exposing (dropdown)

import Html exposing (Html, text, select, option)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)

dropdown: List (String, String) -> (String -> Bool) -> (String -> msg) -> Html msg
dropdown options validation message =
    select 
        [ onInput message ] 
        (List.map (\(val, label) -> option [ value val, selected (validation val) ] [ text label ]) options)
