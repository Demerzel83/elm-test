module View exposing (..)

import Html exposing (Html, div, text, input, select, option)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Query exposing (Query, Select, SelectItem, SelectFieldDef)
import Messages exposing (Msg)

view : Query -> Html Msg
view query =
    div []
        (List.map showSelect query.query.select )

showSelect : SelectItem -> Html Msg
showSelect select =
    let
        divSelectElement = getSelectElement select
    in
        div []
            [ div [] [ text ("Name: "  ++ (getSelectContent select)) ],
                divSelectElement (\def -> def.path) "Path",
                divSelectElement (\def -> def.fn) "Fn",
                fnSelect select,
                divSelectElement (\def -> def.alias) "Alias",
                div [] [ input [ type_ "text", value (getSelectItemAlias select), placeholder "Alias", onInput (Messages.AliasChange select) ] []],
                divSelectElement (\def -> def.label) "Label"
            ]

fnSelect: SelectItem -> Html Msg
fnSelect selectItem = 
    div[] [
        select [ onInput (Messages.FunctionChange selectItem) ] [
            option [ value "max" ] [ text "Max"],
            option [ value "min" ] [ text "Min"],
            option [ value "avg" ] [ text "Average"]
        ]
    ]

getSelectItemAlias : SelectItem -> String
getSelectItemAlias selectItem =
    case selectItem of
        Query.SelectField def -> 
            case def.alias of
                Nothing -> ""
                Just x -> x
        Query.SelectExpression def ->
            def.alias

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