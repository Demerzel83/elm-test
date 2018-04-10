module Main exposing (..)

import Html exposing (beginnerProgram)
import Query exposing (Query, initialModel)
import View exposing (view)
import Messages exposing (Msg)

update : Msg -> Query -> Query
update msg model = 
    case msg of
        Messages.AliasChange select alias -> 
            let 
                oldSelect = model.query.select
                oldQuery = model.query
                newQuery = 
                    { oldQuery 
                    | select = List.map (mapSelectItem select alias) oldSelect }
            in
                { model | query = newQuery } 
        Messages.FunctionChange select fn ->
            let 
                oldSelect = model.query.select
                oldQuery = model.query
                newQuery = 
                    { oldQuery 
                    | select = List.map (mapSelectItemFn select fn) oldSelect }
            in
                { model | query = newQuery }


mapSelectItem : Query.SelectItem -> String -> Query.SelectItem -> Query.SelectItem
mapSelectItem newSelectItem newAlias selectItem =
    case (selectItem, newSelectItem) of
        (Query.SelectField oldDef, Query.SelectField newDef) -> 
            case (oldDef.alias, newDef.alias) of
                (Just x, Just y) -> 
                    if oldDef.name == newDef.name  
                    then Query.SelectField { oldDef | alias = (Just newAlias) } 
                    else selectItem
                (Nothing, _) -> selectItem
                (_, Nothing) -> selectItem
        (Query.SelectExpression oldDef, Query.SelectExpression newDef) ->
            if oldDef.alias == newDef.alias  
            then Query.SelectExpression { oldDef | alias =  newAlias } 
            else selectItem
        (_,_) -> selectItem

mapSelectItemFn : Query.SelectItem -> String -> Query.SelectItem -> Query.SelectItem
mapSelectItemFn newSelectItem newFn selectItem =
    case (selectItem, newSelectItem) of
        (Query.SelectField oldDef, Query.SelectField newDef) -> 
            case (oldDef.alias, newDef.alias) of
                (Just x, Just y) -> 
                    if oldDef.name == newDef.name  
                    then Query.SelectField { oldDef | fn = (Just newFn) } 
                    else selectItem
                (Nothing, _) -> selectItem
                (_, Nothing) -> selectItem
        (_,_) -> selectItem

main : Program Never Query Msg
main =
    beginnerProgram
        { view = view
        , update = update
        , model = initialModel 
        }