module Query.Join.View exposing (showJoins)

import Html exposing (Html, div, text, input, select, option)
import Query exposing (..)
import Query.Messages exposing (..)
import Widgets.Dropdown exposing (dropdown)

showJoins: Maybe Joins -> Html Msg
showJoins joins =
    case joins of
        Nothing -> text "No joins"
        Just (Joins items) -> div [] (List.indexedMap showJoin items)

showJoin: Int -> JoinItem -> Html Msg
showJoin index joinItem =
    div []
    [ div [] [ text ("Column: " ++ joinItem.colum) ]
    , div [] [ text (Maybe.withDefault "NoEntity" joinItem.entity) ]
    , div [] [ 
        case joinItem.joins of 
            Nothing -> text "No more joins" 
            j -> showJoins j]
    , div [] [ 
        case joinItem.joinType of   
            Nothing -> text "Inner"
            Just jtype -> 
                (dropdown 
                    [("Inner", "Inner"), ("Left", "Left") , ("Right", "Right")] 
                    (\o -> case jtype of
                            Inner -> o == "Inner"
                            Left -> o == "Left"
                            Right -> o  == "Right") 
                    (JoinChange joinItem)) ]]
        