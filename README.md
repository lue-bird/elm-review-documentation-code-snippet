# elm-review-documentation-code-snippet

[🔧 `Review.Documentation.CodeSnippet.check`](https://package.elm-lang.org/packages/lue-bird/elm-review-documentation-example/1.0.0/Review-Documentation-CodeSnippet#check "provides fixes")
automatically generates tests from the examples in your documentation (readme, module header, declaration comments).
```elm
{-| `Dict.keys` but returning a `Set` instead of a `List`.

    import Dict
    import Set

    Dict.fromList [ ( 0, "A" ), ( 1, "B" ), ( 2, "C" ) ]
        |> keySet
    --> Set.fromList [ 0, 1, 2 ]

-}
keySet = ...
```
which will generate a test with
```elm
Dict.fromList [ ( 0, "A" ), ( 1, "B" ), ( 2, "C" ) ]
    |> keySet
    |> Expect.equal (Set.fromList [ 0, 1, 2 ])
```
If you know [`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples), you also know how this works.
There are only a few extras like checking for types you'll get only by using this rule instead.

```elm
import Review.Rule
import Review.Documentation.Example

config : List Review.Rule.Rule
config =
    [ Review.Documentation.CodeSnippet.check
    ]
```
I suggest running it in the background
```noformatingples
elm-review --rules Review.Documentation.CodeSnippet --watch --fix-all-without-prompt
```
and adding `tests/DocumentationCodeSnippet.Test.elm` to `.gitignore`.

## why?

Finding broken or incorrect examples in the documentation is confusing and frustrating to new users.
At the same time, these examples quickly get out of sync with your API.
Now, how do you find all the places where things changed for your examples?
The compiler certainly doesn't check them which makes it easy to miss some

## possible ideas for the future

  - Make up fuzzy check syntax. Something like
    ```elm
    --* xs is list unit
    List.take 3 xs |> List.length
    --> Basics.maximum 3 (xs |> List.length)

    --* xs, ys is list unit
    List.map2 Tuple.pair xs ys |> List.length
    --> Basics.minimum (xs |> List.length) (ys |> List.length)
    ```
    where `list unit` is interpreted as `Fuzz.list (Fuzz.constant ())`

## thanks
  - Christoph Hermann (stoeffel) for [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples)
    which established the core ideas and syntax in a nice command line tool
  - dillonkearns for [elm-markdown](https://dark.elm.dmy.fr/packages/dillonkearns/elm-markdown/latest/) of which parts are used as a base for finding code blocks
  - Rupert Smith for [elm-syntax-dsl](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest) which can pretty print a whole elm file and is compatible with `elm-syntax`
  - miniBill for [elm-fast-dict](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest)
