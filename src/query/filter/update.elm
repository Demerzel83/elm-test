module Query.Filter.Update exposing (updateFilter)

import Query exposing (..)
import Query.Messages exposing (..)

updateFilter : Msg -> Query -> Query
updateFilter msg model = 
    case msg of
        FilterValueChange filteRuleDef newValue -> 
            let 
                newFilter = getFilter filteRuleDef newValue model.query.filter
                oldQuery = model.query
                newQuery = { oldQuery | filter = newFilter}
            in
                { model | query = newQuery }
        _ -> model

getFilter : FilterRuleDef -> String -> Maybe Filter -> Maybe Filter
getFilter filteRuleDef newValue filter  = 
    case filter of
        Nothing -> filter
        Just f -> Just (mapFilter filteRuleDef newValue f)

mapFilter: FilterRuleDef -> String -> Filter -> Filter
mapFilter filterRuleDef newValue filter =
    case filter of
        FilterRule filterDefiniton -> 
            if filterDefiniton == filterRuleDef 
            then FilterRule ({ filterDefiniton | value = (Just newValue) })
            else FilterRule filterDefiniton
        FilterAnd filterAndDef -> FilterAnd ({ filterAndDef | and = (List.map (mapFilter filterRuleDef newValue) filterAndDef.and)})
        FilterOr filterOrDef -> FilterOr ({ filterOrDef | or = (List.map (mapFilter filterRuleDef newValue) filterOrDef.or)})
