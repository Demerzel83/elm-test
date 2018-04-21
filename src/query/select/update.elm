module Query.Select.Update exposing (updateSelect)

import Query exposing (..)
import Query.Messages exposing (..)

updateSelect : Msg -> Query -> Query
updateSelect msg model = 
    let
        mapModel = mapQuery model
    in
        case msg of
            AliasChange index alias -> 
                mapModel (mapSelectItem index (mapSelectItemAlias alias))
            FunctionChange index fn ->
                mapModel (mapSelectItem index (mapSelectItemFn fn))
            ExpressionChange index expression ->
                mapModel (mapSelectItem index (mapSelectItemExp expression))
            _ -> model
            
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
        SelectField sf -> SelectField { sf | fn = (Just newFn) }
        se -> se 

mapSelectItemExp : String -> SelectItem -> SelectItem
mapSelectItemExp newExp selectItem =
    case selectItem of
        SelectExpression se -> SelectExpression { se | expression = newExp } 
        sf -> sf

mapSelectItem : Int -> (SelectItem -> SelectItem) -> Int -> SelectItem -> SelectItem
mapSelectItem indexChanged mapSelectItem index oldSelectItem =
    if index == indexChanged 
    then mapSelectItem oldSelectItem
    else oldSelectItem