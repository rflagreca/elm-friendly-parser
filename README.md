# elm-friendly-parser

**Disclaimer:** Work is either in progress or abandoned. Proceed only if you are really curious.

The _fully working version_, where all [tests](./tree/master/test) pass, lies under the [tag `before-docs-and-refactorings`](./tree/before-docs-and-refactorings/), but the `Parser.elm` is just one huge file there.

Current version is a work on splitting the API into proper modules and [some unfinished docs](./blob/master/src/docs.elm.tmp).

The (Execute.elm)[./blob/master/src/Execute.elm] file could be interesting for someone, since it's the implementation of all possible parser operators on functions :).

However, I see there are a lot of awesome parsers and parser-generators do exist for Elm already. This project really helped me in discovering Elm hidden tricks, so I may use them for other things (and also it teached me to use `elm-format` :) ) and, probably, just move on.

The idea was to tranfer the idea [of `pegjs-fn`](http://shamansir.github.io/blog/articles/generating-functional-parsers/), "functional" parser generator to Elm, so the parser grammar could look like this:

```elm
type alias ReturnType = String


rules : Rules ReturnType
rules =
    [ ( "phoneNumber"
      , action
        ( seqnc
            [ maybe (call "prefix")
            , maybe (call "operator")
            , (call "local")
            ]
        )
        (\val _ -> extractPhoneNumber val)
      )
    , ( "prefix"
      , action
        ( seqnc
            [ match "+"
            , some (re "[0-9]")
            ]
        )
        (\val _ -> extractPrefix val)
      )
    , ( "operator"
      , action
        ( seqnc
            [ choice [ match "(", match "[" ]
            , some (re "[0-9]")
            , choice [ match "]", match ")" ]
            ]
        )
        (\val _ -> extractOperator val)
      )
    , ( "local"
      , action
        ( seqnc
            [ some (re "[0-9]")
            , match "-"
            , some (re "[0-9]")
            , match "-"
            , some (re "[0-9]")
            ]
        )
        (\val _ -> extractLocal val)
      )
    ]


init : Parser ReturnType
init =
       Parser.withRules rules
    |> Parser.setStartRule "phoneNumber"


test : ParseResult ReturnType
test =
    init |> Parser.parse "35[057]776-22-13"
```

This way to define parser also allows user to export them to PEG format easily.

See [samples](./tree/master/test/samples) (or [samples from previous version](./tree/before-docs-and-refactorings/test/samples)) for more examples.

Thanks Elm for its beauty and recursive data structures.
