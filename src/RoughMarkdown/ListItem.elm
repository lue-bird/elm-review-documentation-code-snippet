module RoughMarkdown.ListItem exposing (Completion(..), ListItem(..), parser)

import Char.LocalExtra
import Parser
import Parser.Advanced exposing ((|.), (|=))
import Parser.LocalExtra


type ListItem
    = TaskItem Completion String
    | PlainItem String
    | EmptyItem


type Completion
    = Incomplete
    | Complete


parser : Parser.Advanced.Parser String Parser.Problem ListItem
parser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed TaskItem
            |= taskItemParser
            |. Parser.Advanced.chompWhile Char.LocalExtra.isSpaceOrTab
        , Parser.Advanced.succeed PlainItem
        ]
        |= Parser.Advanced.getChompedString Parser.LocalExtra.chompUntilLineEndOrEnd
        |. Parser.LocalExtra.lineEndOrEnd


taskItemParser : Parser.Advanced.Parser String Parser.Problem Completion
taskItemParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed Complete
            |. Parser.Advanced.symbol (Parser.Advanced.Token "[x] " (Parser.ExpectingSymbol "[x] "))
        , Parser.Advanced.succeed Complete
            |. Parser.Advanced.symbol (Parser.Advanced.Token "[X] " (Parser.ExpectingSymbol "[X] "))
        , Parser.Advanced.succeed Incomplete
            |. Parser.Advanced.symbol (Parser.Advanced.Token "[ ] " (Parser.ExpectingSymbol "[ ] "))
        ]
