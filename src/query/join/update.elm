module Query.Join.Update exposing (updateJoin)

import Query exposing (..)
import Query.Messages exposing (..)

updateJoin : Msg -> Query -> Query
updateJoin msg model = 
    case msg of
        JoinChange joinItem join -> 
            let 
                newJoins = mapJoinItem model.query.joins joinItem join
                oldQuery = model.query
                newQuery = { oldQuery | joins = newJoins}
            in
                { model | query = newQuery}
        _ -> model

mapJoinItem : Maybe Joins -> JoinItem -> String -> Maybe Joins
mapJoinItem joins joinItem joinType =
    case joins of
        Nothing -> Just (Joins [{ colum = "test", joinType = (Just Left), entity = Nothing, joins = Nothing }])
        Just (Joins jns) -> Just (Joins (List.map (\j -> 
            if j == joinItem 
            then { j | joinType = (Just (mapJoinType joinType)) } 
            else case j.joins of
                Nothing -> j
                jns -> {j | joins = (mapJoinItem jns joinItem joinType)} ) jns))

mapJoinType : String -> JoinType
mapJoinType joinType =
    case joinType of
        "Inner" -> Inner
        "Right" -> Right
        "Left" -> Left
        _ -> Inner
