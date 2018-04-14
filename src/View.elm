module View exposing (..)

import Html exposing (Html, div, text, input, select, option)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Query exposing (..)
import Messages exposing (Msg)
import Widgets.Dropdown exposing (dropdown)


view : Query -> Html Msg
view query =
    div []
        (List.indexedMap showSelect query.query.select )

showSelect : Int -> SelectItem -> Html Msg
showSelect index select =
    let
        divSelectElement = getSelectElement select
    in
    case select of 
        SelectField def ->
            div []
                [ div [] [ text "------------------------------------------"]
                    , div [][ text ("Name: "  ++ (getSelectContent select)) ]
                    , divSelectElement (\def -> def.path) "Path"
                    , divSelectElement (\def -> def.fn) "Fn"
                    , fnSelect index select
                    , divSelectElement (\def -> def.alias) "Alias"
                    , div [] [ input [ type_ "text", value (getSelectItemAlias select), placeholder "Alias", onInput (Messages.AliasChange index) ] []]
                    , divSelectElement (\def -> def.label) "Label"
                ]
        SelectExpression def ->
            div []
                [ div [] [ text "------------------------------------------"]
                    , div [][ text ("Name: "  ++ (getSelectContent select)) ]
                    , div [] [ input [ type_ "text", value def.expression, placeholder "Expression", onInput (Messages.ExpressionChange index) ] []]
                    , divSelectElement (\def -> def.alias) "Alias"
                    , div [] [ input [ type_ "text", value def.alias, placeholder "Alias", onInput (Messages.AliasChange index) ] []]
                    , divSelectElement (\def -> def.label) "Label"
                ]
fnSelect: Int -> SelectItem -> Html Msg
fnSelect index selectItem = 
    div[] [
        case selectItem of
            SelectField field -> dnDropdown index field selectItem
            SelectExpression _ -> text ""
    ]

fnOptions : List (String, String)
fnOptions = 
    [ ("", ""), ("max", "Max"), ("min", "Min"), ("avg", "Average") ]

dnDropdown: Int -> SelectFieldDef -> SelectItem -> Html Msg
dnDropdown index field selectItem =
    dropdown fnOptions (\val -> field.fn == Just val) (Messages.FunctionChange index)

getSelectItemAlias : SelectItem -> String
getSelectItemAlias selectItem =
    case selectItem of
        Query.SelectField def -> 
            Maybe.withDefault "" def.alias
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
        Query.SelectExpression def -> 
            if param /= "Alias"
            then "No " ++ param ++ " for expressions"
            else def.alias

getText : Maybe String -> String -> String
getText m param =
    Maybe.withDefault ("No " ++ param) m 