module Messages exposing (..)

type Msg
    = AliasChange Int String
    | FunctionChange Int String
    | ExpressionChange Int String