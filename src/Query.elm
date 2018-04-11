module Query exposing (..)

type alias SelectFieldDef =
    { name: String
    , path: Maybe String
    , fn: Maybe String
    , alias: Maybe String
    , label: Maybe String
    }

type alias SelectExpressionDef =
    { expression: String
    , alias: String
    , label: Maybe String
    }

type SelectItem 
    = SelectField SelectFieldDef
    | SelectExpression SelectExpressionDef

type alias Select = List SelectItem

type alias OrderFieldDef =
    { name: String
    , path: Maybe String
    , fn: Maybe String
    , desc: Maybe Bool
    }

type alias OrderExpressionDef =
    { expression: String
    , desc: Maybe Bool
    }

type OrderItem 
    = OrderField OrderFieldDef
    | OrderExpression OrderExpressionDef

type alias Order = List OrderItem

type alias GroupFieldDef =
    { name: String
    , path: Maybe String
    }

type alias GroupExpressionDef =
    { expression: String
    }

type GroupItem 
    = GroupField GroupFieldDef
    | GroupExpression GroupExpressionDef

type alias Group = List GroupItem

type JoinType 
    = Inner 
    | Left 
    | Right

type alias JoinItem =
    { colum: String
    , entity: Maybe String
    , joins: Maybe Joins
    , joinType: Maybe JoinType
    }

type Joins = List JoinItem  

type alias FilterRuleDef a =
    { name: String
    , path: Maybe String
    , op: String
    , fn: Maybe String
    , value: a
    }

type alias FilterAndDef a =
    { and: List Filter a }

type alias FilterOrDef a =
    { or: List Filter a }

type Filter a
    = FilterRule (FilterRuleDef a)
    | FilterAnd (FilterAndDef a)
    | FilterOr (FilterOrDef a)

type Having = Filter

type alias QueryPart =
    { select: Select
    , order:  Maybe Order
    , group:  Maybe Group
    , joins:  Maybe Joins
    , filter: Maybe Filter
    , having: Maybe Having
    }

type alias Query =
    { entity : String
    , query: QueryPart
    }

initialModel : Query
initialModel = 
    { entity = "Test"
    , query =  
        { select = 
            [ SelectField
                { name  = "column_name"
                , path  = Just "/path"
                , fn    = Just "avg"
                , alias = Just "Test"
                , label = Just "Label"
                },
            SelectField
                { name  = "column_name2"
                , path  = Just "/path"
                , fn    = Nothing
                , alias = Just "Test2"
                , label = Nothing
                }
            ]
        , order  = Nothing
        , group  = Nothing
        , joins  = Nothing
        , filter = Nothing
        , having = Nothing
     }
    }