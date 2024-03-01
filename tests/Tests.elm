module Tests exposing (tests)

import Review.Documentation.CodeSnippet
import Review.Test
import Test exposing (Test)


tests : Test
tests =
    Test.describe "elm-review-opaque-type"
        [ Test.test "reports missing DocumentationCodeSnippet.Test module"
            (\() ->
                """module A exposing (a)

a =
    a
"""
                    |> Review.Test.run Review.Documentation.CodeSnippet.check
                    |> Review.Test.expect
                        [ Review.Test.globalErrors
                            [ { message = "documentation code snippet test module needs to be added"
                              , details =
                                    [ "We need a module to generate documentation code snippet tests in. Please add a module tests/DocumentationCodeSnippet/Test.elm."
                                    ]
                              }
                            ]
                        ]
            )
        , Test.test "creates \"empty\" test file when no code snippets exist and DocumentationCodeSnippet.Test module exists"
            (\() ->
                [ """module A exposing (a)

a =
    a
"""
                , """module DocumentationCodeSnippet.Test exposing (tests)

tests =
    tests
"""
                ]
                    |> Review.Test.runOnModules
                        Review.Documentation.CodeSnippet.check
                    |> Review.Test.expectErrorsForModules
                        [ ( "DocumentationCodeSnippet.Test"
                          , [ Review.Test.error
                                { message = "documentation code snippet test can be added"
                                , details =
                                    [ "Adding them will help verify that code blocks in your readme and module documentation work correctly."
                                    ]
                                , under = "module"
                                }
                                |> Review.Test.whenFixed
                                    """module DocumentationCodeSnippet.Test exposing (tests)

{-| automatically generated by [elm-review-documentation-code-snippet](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-documentation-code-snippet/latest)

-}

import Expect
import Test


tests : Test.Test
tests =
    Test.describe
        "documentation code snippets"
        [ Test.test
            "currently none. Since having no code snippets is perfectly fine, adding this simple test tells elm-test that everything's good (empty tests fail or throw warnings)"
            (\\() -> Expect.pass)
        ]
"""
                            ]
                          )
                        ]
            )
        , Test.test "creates filled test file when code snippet with same-line --> exists and DocumentationCodeSnippet.Test module exists"
            (\() ->
                [ """module A exposing (plus1)

{-| + 1.


    1 |> A.plus1 --> 2

-}
plus1 n =
    n + 1
"""
                , """module DocumentationCodeSnippet.Test exposing (tests)

tests =
    tests
"""
                ]
                    |> Review.Test.runOnModules
                        Review.Documentation.CodeSnippet.check
                    |> Review.Test.expectErrorsForModules
                        [ ( "DocumentationCodeSnippet.Test"
                          , [ Review.Test.error
                                { message = "documentation code snippet test can be added"
                                , details =
                                    [ "Adding them will help verify that code blocks in your readme and module documentation work correctly."
                                    ]
                                , under = "module"
                                }
                                |> Review.Test.whenFixed
                                    """module DocumentationCodeSnippet.Test exposing (tests)

{-| automatically generated by [elm-review-documentation-code-snippet](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-documentation-code-snippet/latest)

-}

import A
import Expect
import Test


tests : Test.Test
tests =
    Test.describe
        "documentation code snippets"
        [ Test.describe
            "A"
            [ Test.describe
                "plus1"
                [ Test.describe
                    "code snippet 0"
                    [ Test.test "0" (\\() -> (1 |> A.plus1) |> Expect.equal 2) ]
                ]
            ]
        ]
"""
                            ]
                          )
                        ]
            )
        , Test.test "creates filled test file when code snippet with next-line --> exists and DocumentationCodeSnippet.Test module exists"
            (\() ->
                [ """module A exposing (plus1)

{-| + 1.


    1 |> plus1
    --> 2

-}
plus1 n =
    n + 1
"""
                , """module DocumentationCodeSnippet.Test exposing (tests)

tests =
    tests
"""
                ]
                    |> Review.Test.runOnModules
                        Review.Documentation.CodeSnippet.check
                    |> Review.Test.expectErrorsForModules
                        [ ( "DocumentationCodeSnippet.Test"
                          , [ Review.Test.error
                                { message = "documentation code snippet test can be added"
                                , details =
                                    [ "Adding them will help verify that code blocks in your readme and module documentation work correctly."
                                    ]
                                , under = "module"
                                }
                                |> Review.Test.whenFixed
                                    """module DocumentationCodeSnippet.Test exposing (tests)

{-| automatically generated by [elm-review-documentation-code-snippet](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-documentation-code-snippet/latest)

-}

import A
import Expect
import Test


tests : Test.Test
tests =
    Test.describe
        "documentation code snippets"
        [ Test.describe
            "A"
            [ Test.describe
                "plus1"
                [ Test.describe
                    "code snippet 0"
                    [ Test.test "0" (\\() -> (1 |> A.plus1) |> Expect.equal 2) ]
                ]
            ]
        ]
"""
                            ]
                          )
                        ]
            )
        , Test.test "creates filled test file when code snippet with multiple next-line --> exists and DocumentationCodeSnippet.Test module exists"
            (\() ->
                [ """module A exposing (plus1And2)

{-| + 1.


    1 |> plus1And2
    --> ( 2
    --> , 3
    --> )

-}
plus1And2 n =
    ( n + 1, n + 2 )
"""
                , """module DocumentationCodeSnippet.Test exposing (tests)

tests =
    tests
"""
                ]
                    |> Review.Test.runOnModules
                        Review.Documentation.CodeSnippet.check
                    |> Review.Test.expectErrorsForModules
                        [ ( "DocumentationCodeSnippet.Test"
                          , [ Review.Test.error
                                { message = "documentation code snippet test can be added"
                                , details =
                                    [ "Adding them will help verify that code blocks in your readme and module documentation work correctly."
                                    ]
                                , under = "module"
                                }
                                |> Review.Test.whenFixed
                                    """module DocumentationCodeSnippet.Test exposing (tests)

{-| automatically generated by [elm-review-documentation-code-snippet](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-documentation-code-snippet/latest)

-}

import A
import Expect
import Test


tests : Test.Test
tests =
    Test.describe
        "documentation code snippets"
        [ Test.describe
            "A"
            [ Test.describe
                "plus1And2"
                [ Test.describe
                    "code snippet 0"
                    [ Test.test
                        "0"
                        (\\() -> (1 |> A.plus1And2) |> Expect.equal ( 2, 3 ))
                    ]
                ]
            ]
        ]
"""
                            ]
                          )
                        ]
            )
        , Test.test "creates filled test file with location prefixed declarations when code snippet with declaration exists and DocumentationCodeSnippet.Test module exists"
            (\() ->
                [ """module A exposing (toggle)

{-| + 1.


    type Toggle
        = On
        | Off
    
    On |> toggle
    --> Off

-}
toggle t =
    case t of
        On ->
            Off
        
        Off ->
            On
"""
                , """module DocumentationCodeSnippet.Test exposing (tests)

tests =
    tests
"""
                ]
                    |> Review.Test.runOnModules
                        Review.Documentation.CodeSnippet.check
                    |> Review.Test.expectErrorsForModules
                        [ ( "DocumentationCodeSnippet.Test"
                          , [ Review.Test.error
                                { message = "documentation code snippet test can be added"
                                , details =
                                    [ "Adding them will help verify that code blocks in your readme and module documentation work correctly."
                                    ]
                                , under = "module"
                                }
                                |> Review.Test.whenFixed
                                    """module DocumentationCodeSnippet.Test exposing (tests)

{-| automatically generated by [elm-review-documentation-code-snippet](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-documentation-code-snippet/latest)

-}

import A
import Expect
import Test


tests : Test.Test
tests =
    Test.describe
        "documentation code snippets"
        [ Test.describe
            "A"
            [ Test.describe
                "toggle"
                [ Test.describe
                    "code snippet 0"
                    [ Test.test
                        "0"
                        (\\() ->
                            (A__toggle_0__On |> A.toggle)
                                |> Expect.equal A__toggle_0__Off
                        )
                    ]
                ]
            ]
        ]


type A__toggle_0__Toggle
    = A__toggle_0__On
    | A__toggle_0__Off
"""
                            ]
                          )
                        ]
            )
        ]
