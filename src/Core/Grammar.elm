module Core.Grammar exposing
    ( RuleName
    , Rules
    , Grammar
    )

import Dict exposing (..)

import Core.Operator exposing (Operator)

type alias RuleName = String
type alias Rules o = Dict RuleName (Operator o)
-- FIXME: Rename to Grammar?
type alias Grammar o = List ( RuleName, Operator o )
