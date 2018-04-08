module View exposing (..)

import Html exposing (Html, div, text)
import Query exposing (Query, Select, SelectItem, SelectFieldDef)

view : Query -> Html msg
view query =
    div []
        (List.map showSelect query.query.select )

showSelect : SelectItem -> Html msg
showSelect select =
    let
        divSelectElement = getSelectElement select
    in
        div []
            [ div [] [ text ("Name: "  ++ (getSelectContent select)) ],
                divSelectElement (\def -> def.path) "Path",
                divSelectElement (\def -> def.fn) "Fn",
                divSelectElement (\def -> def.alias) "Alias",
                divSelectElement (\def -> def.label) "Label"
            ]

getSelectElement: SelectItem -> (SelectFieldDef -> Maybe String) -> String -> Html msg 
getSelectElement select fn param =
    div [] [ text (param ++ ": "  ++ (select |> getSelectField fn param)) ]

getSelectContent: SelectItem -> String
getSelectContent select =
    case select of
        Query.SelectField def -> def.name
        Query.SelectExpression def -> def.expression

getSelectField: (SelectFieldDef -> Maybe String) -> String -> SelectItem -> String
getSelectField fn param select =
    case select of
        Query.SelectField def -> getText (fn def) param 
        Query.SelectExpression def -> "No " ++ param ++ " for expressions"

getText : Maybe String -> String -> String
getText m param =
    case m of
        Just x -> x
        Nothing -> "No " ++ param