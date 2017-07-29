module Operator exposing
    ( Operator(..)
    , ch, match, choice, seqnc, maybe, text, any, some, and, not
    , action, pre, xpre, label, call, re, redesc
    , Rule, RuleName
    )

import Action exposing
    ( ActionResult(..)
    , PrefixActionResult(..)
    , UserCode
    , UserPrefixCode
    )

type alias OpLabel = String
type alias RuleName = String
type alias Rule o = ( RuleName, Operator o )

type Operator o =
      NextChar -- 1. `ch`
    | Match String -- 2. `match`
    | Regex String (Maybe String) -- 3. `re`, `redesc`
    | TextOf (Operator o) -- 4. `text`
    | Maybe_ (Operator o) -- 5. `maybe`
    | Some (Operator o) -- 6. `some`
    | Any (Operator o)  -- 7. `any`
    | And (Operator o) -- 8. `and`
    | Not (Operator o) -- 9. `not`
    | Sequence (List (Operator o)) -- 10. `seqnc`
    | Choice (List (Operator o)) -- 11. `choice`
    | Action (Operator o) (UserCode o) -- 12. `action`
    | PreExec (UserPrefixCode o) -- 13. `pre`
    | NegPreExec (UserPrefixCode o) -- 14. `xpre`
    | Label String (Operator o) -- 15. `label`
    -- | Rule RuleName (Operator o) -- 16. `rule`
    | Call RuleName -- 17. `call` a.k.a `ref`
    -- | Alias String (Operator o) -- 18. `as`
    | CallAs RuleName RuleName

match : String -> Operator o
match subject =
    Match subject

ch : Operator o
ch =
    NextChar

re : String -> Operator o
re regex_ =
    Regex regex_ Nothing

redesc : String -> String -> Operator o
redesc regex_ description =
    Regex regex_ (Just description)

seqnc : List (Operator o) -> Operator o
seqnc operators =
    Sequence operators

choice : List (Operator o) -> Operator o
choice operators =
    Choice operators


maybe : Operator o -> Operator o
maybe operator =
    Maybe_ operator

any : Operator o -> Operator o
any operator =
    Any operator

some : Operator o -> Operator o
some operator =
    Some operator

and : Operator o -> Operator o
and operator =
    And operator

not : Operator o -> Operator o
not operator =
    Not operator

-- FIXME: make `call` accept the rule from the RulesList
call : RuleName -> Operator o
call ruleName =
    Call ruleName

-- FIXME: actions should have access to a position, check the examples.
action : Operator o -> UserCode o -> Operator o
action operator userCode =
    Action operator userCode

pre : UserPrefixCode o -> Operator o
pre userCode =
    PreExec userCode

xpre : UserPrefixCode o -> Operator o
xpre userCode =
    NegPreExec userCode

text : Operator o -> Operator o
text operator =
    TextOf operator

-- FIXME: check the examples
label : String -> Operator o -> Operator o
label name operator =
    Label name operator

