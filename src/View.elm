module View exposing (..)

import Html exposing (Html, div, text, input, select, option)
import Query exposing (..)
import Query.View exposing (showQuery)
import Query.Messages exposing (Msg)

view : Query -> Html Msg
view = showQuery
