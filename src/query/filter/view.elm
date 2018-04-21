module Query.Filter.View exposing (showFilter)

import Html exposing (Html, div, text, input, select, option)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Query exposing (..)
import Query.Messages exposing (..)

showFilter: Maybe Filter -> Html Msg
showFilter filter =
    case filter of
        Nothing -> text "No Filter"
        Just f -> parseFilter f

parseFilter: Filter -> Html Msg
parseFilter filter =
    case filter of
        FilterRule filterRuleDef -> showFilterRuleDef filterRuleDef 
        FilterAnd filterAndDef -> showFilterAnd filterAndDef
        FilterOr filterOrDef -> showFilterOr filterOrDef

showFilterAnd: FilterAndDef -> Html Msg
showFilterAnd { and } =
    div [] (List.map parseFilter and)

showFilterOr: FilterOrDef -> Html Msg
showFilterOr { or } =
    div [] (List.map parseFilter or)

showFilterRuleDef : FilterRuleDef -> Html Msg
showFilterRuleDef filteRuleDef =
    let 
        { name, path, op, fn, value } = filteRuleDef 
    in
        div [] 
            [ text ("Name: " ++ name)
            , text (" Operation: " ++ op)
            , text (" Path: " ++ (toString path))
            , text (" fn: " ++ (toString fn))
            , showInputValue filteRuleDef
            ]

showInputValue: FilterRuleDef -> Html Msg
showInputValue filteRuleDef =
    case filteRuleDef.value of
        Nothing -> text "No value"
        Just val -> input [ type_ "text", value val, placeholder "Value", onInput (FilterValueChange filteRuleDef)] []
