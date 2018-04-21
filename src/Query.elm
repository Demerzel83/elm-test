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

type Joins = Joins (List JoinItem)  

type alias FilterRuleDef =
    { name: String
    , path: Maybe String
    , op: String
    , fn: Maybe String
    , value: Maybe String 
    }

type alias FilterAndDef =
    { and: List Filter }

type alias FilterOrDef  =
    { or: List Filter }

type Filter 
    = FilterRule FilterRuleDef 
    | FilterAnd FilterAndDef 
    | FilterOr FilterOrDef 

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

getFilter : Maybe Filter 
getFilter = 
       Just (FilterAnd {
           and = [
               FilterRule (
                    { name = "description"
                    , path = Nothing
                    , op = "eq"
                    , fn = Nothing
                    , value = Just "lalala"
                    }),
                FilterRule (
                    { name = "description2"
                    , path = Nothing
                    , op = "eq"
                    , fn = Nothing
                    , value = Just "1"
                    }) 
           ] 
       })
           

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
                }
            , SelectField
                { name  = "column_name2"
                , path  = Just "/path"
                , fn    = Nothing
                , alias = Just "Test2"
                , label = Nothing
                }
            , SelectExpression
                { expression = "SQL expression"
                , alias = "Alias for the sql expresion"
                , label = Just "Label for the expression"
                }
            ]
        , order  = Nothing
        , group  = Nothing
        , joins  = Just (Joins [   
            { colum = "Column 666"
            , entity = Nothing
            , joins = Just (Joins [   
                { colum = "Column 999"
                , entity = Just "My Entity"
                , joins = Just (Joins [   
                    { colum = "Column 888"
                    , entity = Just "More entity"
                    , joins = Just (Joins [   
                        { colum = "Column 555"
                        , entity = Just "Lalala entity"
                        , joins = Nothing
                        , joinType = Just Right
                        } 
                        ])
                    , joinType = Just Left
                    } 
                    ])
                , joinType = Just Right
                } 
                ])
            , joinType = Nothing
            } 
            ])
        , filter = getFilter
        , having = Nothing
     }
    }