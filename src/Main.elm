module Main exposing (..)

import Html exposing (beginnerProgram)
import Query exposing (..)
import View exposing (view)
import Messages exposing (Msg)

update : Msg -> Query -> Query
update msg model = 
    case msg of
        Messages.AliasChange select alias -> 
            mapQuery model (mapSelectItemAlias alias select)
        Messages.FunctionChange select fn ->
            mapQuery model (mapSelectItemFn fn select)

mapQuery : Query -> (SelectItem -> SelectItem) -> Query
mapQuery model mapSelectItem =
    let 
        oldSelect = model.query.select
        oldQuery = model.query
        newQuery = 
            { oldQuery 
            | select = List.map mapSelectItem oldSelect }
    in
        { model | query = newQuery }

mapSelectItemAlias : String -> SelectItem -> SelectItem -> SelectItem
mapSelectItemAlias newAlias =
    mapSelectItem 
        (\se -> { se | alias =  newAlias }) 
        (\sf -> { sf | alias = (Just newAlias) } ) 

mapSelectItemFn : String -> SelectItem -> SelectItem -> SelectItem
mapSelectItemFn newFn =
    mapSelectItem 
        (\se -> se) 
        (\sf -> { sf | fn = (Just newFn) } ) 

mapSelectItem : (SelectExpressionDef -> SelectExpressionDef) -> (SelectFieldDef -> SelectFieldDef) -> SelectItem -> SelectItem -> SelectItem
mapSelectItem mapSelectExpression mapSelectField newSelectItem selectItem =
    case (selectItem, newSelectItem) of
        (SelectField oldDef, SelectField newDef) -> 
            case (oldDef.alias, newDef.alias) of
                (Just _, Just _) -> 
                    if oldDef.name == newDef.name  
                    then SelectField (mapSelectField oldDef) 
                    else selectItem
                (Nothing, _) -> selectItem
                (_, Nothing) -> selectItem
        (SelectExpression oldDef, SelectExpression newDef) ->
            if oldDef.alias == newDef.alias  
            then SelectExpression (mapSelectExpression oldDef)  
            else selectItem
        (_,_) -> selectItem

main : Program Never Query Msg
main =
    beginnerProgram
        { view = view
        , update = update
        , model = initialModel 
        }