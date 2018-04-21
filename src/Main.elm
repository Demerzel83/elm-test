module Main exposing (..)

import Html exposing (beginnerProgram)
import Query exposing (..)
import View exposing (view)
import Query.Select.Update exposing (updateSelect)
import Query.Filter.Update exposing (updateFilter)
import Query.Join.Update exposing (updateJoin)
import Query.Messages exposing (Msg)

update : Msg -> Query -> Query
update msg model = 
    model 
    |> (updateSelect msg) 
    |> (updateFilter msg) 
    |> (updateJoin msg)
    
main : Program Never Query Msg
main =
    beginnerProgram
        { view = view
        , update = update
        , model = initialModel 
        }