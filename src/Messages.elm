module Messages exposing (..)
import Query exposing (SelectItem)

type Msg
    = AliasChange SelectItem String
    | FunctionChange SelectItem String