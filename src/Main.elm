module Main exposing (..)

import Html exposing (beginnerProgram)
import Query exposing (Query, initialModel)
import View exposing (view)

type Msg = None

update : Msg -> Query -> Query
update msg model = model

main =
    beginnerProgram
        { view = view
        , update = update
        , model = initialModel 
        }