module Query.View exposing (showQuery)

import Html exposing (Html, div, text, input, select, option)
import Query exposing (..)

import Query.Select.View exposing (showSelect)
import Query.Join.View exposing (showJoins)
import Query.Filter.View exposing (showFilter)
import Query.Messages exposing (..)

showQuery: Query -> Html Msg
showQuery query =
    div [] 
        [ div [] (text "SELECT" :: List.indexedMap showSelect query.query.select)
        , text "JOIN______________________________________________________"
        , div [] [ showJoins query.query.joins ]
        , text "FILTER____________________________________________________"
        , div [] [ showFilter query.query.filter ]
        ]
