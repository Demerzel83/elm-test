module Query.Messages exposing (..)

import Query exposing (FilterRuleDef, JoinItem)

type Msg 
    = AliasChange Int String
    | FunctionChange Int String
    | ExpressionChange Int String
    | FilterValueChange FilterRuleDef String
    | JoinChange JoinItem String