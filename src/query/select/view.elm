module Query.Select.View exposing (showSelect)

import Html exposing (Html, div, text, input, select, option)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import Query exposing (..)
import Query.Messages exposing (..)
import Widgets.Dropdown exposing (dropdown)


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
                    , fnSelect index select
                    , div [] [ input [ type_ "text", value (getSelectItemAlias select), placeholder "Alias", onInput (AliasChange index) ] []]
                    , divSelectElement (\def -> def.label) "Label"
                ]
        SelectExpression def ->
            div []
                [ div [] [ text "------------------------------------------"]
                    , div [][ text "Sql expression " ]
                    , div [] [ input [ type_ "text", value def.expression, placeholder "Expression", onInput (ExpressionChange index) ] []]
                    , div [] [ input [ type_ "text", value def.alias, placeholder "Alias", onInput (AliasChange index) ] []]
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
    dropdown fnOptions (\val -> field.fn == Just val) (FunctionChange index)

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