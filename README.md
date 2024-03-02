[🔧 `Review.Documentation.CodeSnippet.check`](https://package.elm-lang.org/packages/lue-bird/elm-review-documentation-example/1.0.0/Review-Documentation-CodeSnippet#check "provides fixes") checks your small code examples in the readme, module headers and declaration comments for valid syntax, matching types and correctness.

To check this, it generates tests from these code snippets
(If you know [`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples), you also know how this works. This rule has only a few extras like checking for types or actually getting errors for invalid syntax.)
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

## why?

Finding broken or incorrect examples in the documentation is confusing and frustrating to new users.
At the same time, these examples quickly get out of sync with your API.
Now, how do you find all the places where things changed for your examples?
The compiler certainly doesn't check them which makes it easy to miss some

## setup

  - ```noformatingples
    elm install lue-bird/elm-review-documentation-code-snippet
    ```
    then add the rule to your `review/src/ReviewConfig.elm`
    ```elm
    import Review.Rule
    import Review.Documentation.CodeSnippet

    config : List Review.Rule.Rule
    config =
        [ Review.Documentation.CodeSnippet.check
        ]
    ```
    or if you don't want to install it, yet
    ```noformatingples
    elm-review --template lue-bird/elm-review-documentation-code-snippet/example
    ```
  - add a minimal `tests/DocumentationCodeSnippetTest.elm` which the rule can overwrite. Something like
    ```elm
    module DocumentationCodeSnippetTest exposing (tests)
    tests =
        tests
    ```
    You can add this file to `.gitignore`.

I suggest running it in the background
```noformatingples
elm-review --rules Review.Documentation.CodeSnippet --watch --fix-all-without-prompt
```
while from time to time keeping an eye on possible reported syntax errors and failing/non-compiling generated tests.

## thanks
  - Christoph Hermann (stoeffel) for [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples)
    which established the core ideas and syntax in a nice command line tool
  - dillonkearns for [elm-markdown](https://dark.elm.dmy.fr/packages/dillonkearns/elm-markdown/latest/) of which parts are used as a base for finding code blocks
  - Rupert Smith for [elm-syntax-dsl](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest) which can pretty print a whole elm file and is compatible with `elm-syntax`
  - miniBill for [elm-fast-dict](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest)

## what could we add in the future?

  - fuzzy check syntax. Something like
    ```elm
    --* xs is list unit
    List.take 3 xs |> List.length
    --> Basics.maximum 3 (xs |> List.length)

    --* xs, ys is list unit
    List.map2 Tuple.pair xs ys |> List.length
    --> Basics.minimum (xs |> List.length) (ys |> List.length)
    ```
    where `list unit` is interpreted as `Fuzz.list (Fuzz.constant ())`
  - compare imports with used module names and automatically add missing imports as a fix
  - ✨ your idea
