module Parser.LocalExtra exposing (chompOneOrMore, chompUntilLineEndOrEnd, endOfFile, lineEnd, lineEndOrEnd, positiveInteger, space, upTo, upToThreeSpaces)

import Char.LocalExtra
import Parser
import Parser.Advanced exposing ((|.))
import Parser.Token


space : Parser.Advanced.Parser String Parser.Problem ()
space =
    Parser.Advanced.token Parser.Token.space



-- A note about the relationship between tabs and spaces:
-- In places where tabs can define block structure, they are to be
-- treated as four spaces i.e. a single tab can create an indented
-- code block.


upToThreeSpaces : Parser.Advanced.Parser String Parser.Problem ()
upToThreeSpaces =
    Parser.Advanced.oneOf
        [ space
            |. Parser.Advanced.oneOf [ space, Parser.Advanced.succeed () ]
            |. Parser.Advanced.oneOf [ space, Parser.Advanced.succeed () ]
        , Parser.Advanced.succeed ()
        ]


{-| From <https://spec.commonmark.org/0.29/#line-ending>:

> is a newline (U+000A), a carriage return (U+000D) not followed by a newline, or a carriage return and a following newline.

-}
lineEnd : Parser.Advanced.Parser String Parser.Problem ()
lineEnd =
    Parser.Advanced.oneOf
        [ Parser.Advanced.token Parser.Token.newline
        , Parser.Advanced.token Parser.Token.carriageReturn
            |. Parser.Advanced.oneOf
                [ Parser.Advanced.token Parser.Token.newline
                , Parser.Advanced.succeed ()
                ]
        ]


endOfFile : Parser.Advanced.Parser String Parser.Problem ()
endOfFile =
    Parser.Advanced.end (Parser.Expecting "the end of the input")


lineEndOrEnd : Parser.Advanced.Parser String Parser.Problem ()
lineEndOrEnd =
    Parser.Advanced.oneOf
        [ lineEnd
        , endOfFile
        ]


chompUntilLineEndOrEnd : Parser.Advanced.Parser String Parser.Problem ()
chompUntilLineEndOrEnd =
    Parser.Advanced.chompWhile (\c -> not (c |> Char.LocalExtra.isLineEnd))


positiveInteger : Parser.Advanced.Parser String Parser.Problem Int
positiveInteger =
    Parser.Advanced.mapChompedString (\str _ -> String.toInt str |> Maybe.withDefault 0)
        (chompOneOrMore Char.isDigit)


chompOneOrMore : (Char -> Bool) -> Parser.Advanced.Parser String Parser.Problem ()
chompOneOrMore condition =
    Parser.Advanced.chompIf condition (Parser.Problem "Expected one or more character")
        |. Parser.Advanced.chompWhile condition


upTo : Int -> Parser.Advanced.Parser String Parser.Problem () -> Parser.Advanced.Parser String Parser.Problem ()
upTo n parser =
    case List.repeat n parser of
        -- chompUpTo 0, so nothing to chomp
        [] ->
            Parser.Advanced.succeed ()

        -- Nests lots of oneOf [parser, succeed].
        -- Based on upToThree. If any of the oneOfs
        -- fail, the whole chain is cut short. This
        -- could be replaced with an implementation
        -- that doesn't preload the list with parsers,
        -- as we already have it as an argument.
        firstParser :: remainingParsers ->
            List.foldl
                (\p parsers -> Parser.Advanced.oneOf [ p |. parsers, Parser.Advanced.succeed () ])
                (Parser.Advanced.oneOf [ firstParser, Parser.Advanced.succeed () ])
                remainingParsers
