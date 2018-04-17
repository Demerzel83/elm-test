module Messages exposing (..)
import Query exposing (..)

type Msg
    = AliasChange Int String
    | FunctionChange Int String
    | ExpressionChange Int String
    | JoinChange JoinItem  String