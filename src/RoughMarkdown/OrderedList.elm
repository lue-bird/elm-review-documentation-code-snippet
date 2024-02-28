module RoughMarkdown.OrderedList exposing (ListItem, OrderedListMarker(..), parser)

import Char.LocalExtra
import Parser
import Parser.Advanced exposing ((|.), (|=))
import Parser.LocalExtra exposing (chompOneOrMore, upTo)
import Parser.Token


type OrderedListMarker
    = Dot
    | Paren


type alias Parser a =
    Parser.Advanced.Parser String Parser.Problem a


type alias ListItem =
    { order : Int
    , intended : Int
    , marker : OrderedListMarker
    , body : String
    }


parser : Bool -> Parser ListItem
parser previousWasBody =
    Parser.Advanced.succeed
        (\start order marker mid ( end, body ) ->
            if (end - mid) <= 4 then
                { order = order
                , intended = end - start
                , marker = marker
                , body = body
                }

            else
                { order = order
                , intended = mid - start + 1
                , marker = marker
                , body = String.repeat (end - mid - 1) " " ++ body
                }
        )
        |= Parser.Advanced.getCol
        |= Parser.Advanced.backtrackable (orderedListOrderParser previousWasBody)
        |= Parser.Advanced.backtrackable orderedListMarkerParser
        |= Parser.Advanced.getCol
        |= (if previousWasBody then
                orderedListItemBodyParser

            else
                Parser.Advanced.oneOf
                    [ orderedListEmptyItemParser
                    , orderedListItemBodyParser
                    ]
           )


orderedListOrderParser : Bool -> Parser.Advanced.Parser String Parser.Problem Int
orderedListOrderParser previousWasBody =
    if previousWasBody then
        Parser.Advanced.succeed identity
            |. upTo 3 Parser.LocalExtra.space
            |= positiveIntegerMaxOf9Digits
            |> Parser.Advanced.andThen
                -- Lists inside a paragraph, or after a paragraph without a line break, must start with index 1.
                (\parsed ->
                    case parsed of
                        1 ->
                            Parser.Advanced.succeed 1

                        _ ->
                            Parser.Advanced.problem (Parser.Problem "Lists inside a paragraph or after a paragraph without a blank line must start with 1")
                )

    else
        Parser.Advanced.succeed identity
            |. upTo 3 Parser.LocalExtra.space
            |= positiveIntegerMaxOf9Digits


positiveIntegerMaxOf9Digits : Parser Int
positiveIntegerMaxOf9Digits =
    Parser.LocalExtra.positiveInteger
        |> Parser.Advanced.andThen
            (\parsed ->
                if parsed <= 999999999 then
                    Parser.Advanced.succeed parsed

                else
                    Parser.Advanced.problem (Parser.Problem "Starting numbers must be nine digits or less.")
            )


orderedListMarkerParser : Parser OrderedListMarker
orderedListMarkerParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed Dot
            |. Parser.Advanced.symbol Parser.Token.dot
        , Parser.Advanced.succeed Paren
            |. Parser.Advanced.symbol Parser.Token.closingParen
        ]


orderedListItemBodyParser : Parser ( Int, String )
orderedListItemBodyParser =
    Parser.Advanced.succeed (\bodyStartPos item -> ( bodyStartPos, item ))
        |. chompOneOrMore Char.LocalExtra.isSpaceOrTab
        |= Parser.Advanced.getCol
        |= Parser.Advanced.getChompedString Parser.LocalExtra.chompUntilLineEndOrEnd
        |. Parser.LocalExtra.lineEndOrEnd


orderedListEmptyItemParser : Parser ( Int, String )
orderedListEmptyItemParser =
    Parser.Advanced.succeed (\bodyStartPos -> ( bodyStartPos, "" ))
        |= Parser.Advanced.getCol
        |. Parser.LocalExtra.lineEndOrEnd
