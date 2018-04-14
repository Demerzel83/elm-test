module Main exposing (..)

import Html exposing (beginnerProgram)
import Query exposing (..)
import View exposing (view)
import Messages exposing (Msg)

update : Msg -> Query -> Query
update msg model = 
    case msg of
        Messages.AliasChange index alias -> 
            mapQuery model (mapSelectItem index (mapSelectItemAlias alias))
        Messages.FunctionChange index fn ->
            mapQuery model (mapSelectItem index (mapSelectItemFn fn))
        Messages.ExpressionChange index expression ->
            mapQuery model (mapSelectItem index (mapSelectItemExp expression))

mapQuery : Query -> (Int -> SelectItem -> SelectItem) -> Query
mapQuery model mapSelectItem =
    let 
        oldSelect = model.query.select
        oldQuery = model.query
        newSelect = List.indexedMap mapSelectItem oldSelect
        newQuery = 
            { oldQuery 
            | select = newSelect }
    in
        { model | query = newQuery }

mapSelectItemAlias : String -> SelectItem -> SelectItem
mapSelectItemAlias newAlias selectItem =
    case selectItem of
        SelectField sf -> SelectField { sf | alias = (Just newAlias) } 
        SelectExpression se -> SelectExpression { se | alias = newAlias }

mapSelectItemFn : String -> SelectItem -> SelectItem
mapSelectItemFn newFn selectItem =
    case selectItem of
        SelectExpression se -> SelectExpression se 
        SelectField sf -> SelectField { sf | fn = (Just newFn) }

mapSelectItemExp : String -> SelectItem -> SelectItem
mapSelectItemExp newExp selectItem =
    case selectItem of
        SelectExpression se -> SelectExpression { se | expression = newExp } 
        SelectField sf -> SelectField sf

mapSelectItem : Int -> (SelectItem -> SelectItem) -> Int -> SelectItem -> SelectItem
mapSelectItem indexChanged mapSelectItem index oldSelectItem =
    if index == indexChanged 
    then mapSelectItem oldSelectItem
    else oldSelectItem

main : Program Never Query Msg
main =
    beginnerProgram
        { view = view
        , update = update
        , model = initialModel 
        }