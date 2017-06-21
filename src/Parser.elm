module Parser exposing (Parser)

{-|

@docs Parser

-}
import Core.Parser as C exposing (..)

{-|
-}
type Parser = Core.Parser

seqnc = C.seqnc
match = C.match
