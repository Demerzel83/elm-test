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
        [ div[] (text "SELECT" :: List.indexedMap showSelect query.query.select)
        , text "_________________________________________________________"
        , div[] [ showJoins query.query.joins ]
        , text "_________________________________________________________"
        , text "_________________________________________________________"
        , text (toString  query.query.joins)
        ]

-- JOIN --------------------------------------------------------------
showJoins: Maybe Joins -> Html Msg
showJoins joins =
    case joins of
        Nothing -> text "No joins"
        Just (Joins items) -> div [] 
            (List.indexedMap showJoin items)

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
                    (Messages.JoinChange joinItem)) ]]
            
            
-- SELECT --------------------------------------------------------------
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
                    , div [] [ input [ type_ "text", value (getSelectItemAlias select), placeholder "Alias", onInput (Messages.AliasChange index) ] []]
                    , divSelectElement (\def -> def.label) "Label"
                ]
        SelectExpression def ->
            div []
                [ div [] [ text "------------------------------------------"]
                    , div [][ text "Sql expression " ]
                    , div [] [ input [ type_ "text", value def.expression, placeholder "Expression", onInput (Messages.ExpressionChange index) ] []]
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