module RoughMarkdown.UnorderedList exposing (UnorderedListMarker(..), parser)

import Char.LocalExtra
import Parser
import Parser.Advanced exposing ((|.), (|=))
import Parser.LocalExtra exposing (chompOneOrMore)
import RoughMarkdown.ListItem exposing (ListItem(..))


type UnorderedListMarker
    = Minus
    | Plus
    | Asterisk


parser : Bool -> Parser.Advanced.Parser String Parser.Problem ( UnorderedListMarker, Int, ListItem )
parser previousWasBody =
    Parser.Advanced.succeed getIntendedCodeItem
        |= Parser.Advanced.getCol
        |= Parser.Advanced.backtrackable unorderedListMarkerParser
        |= Parser.Advanced.getCol
        |= (if previousWasBody then
                unorderedListItemBodyParser

            else
                Parser.Advanced.oneOf
                    [ unorderedListEmptyItemParser
                    , unorderedListItemBodyParser
                    ]
           )


unorderedListMarkerParser : Parser.Advanced.Parser String Parser.Problem UnorderedListMarker
unorderedListMarkerParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed Minus
            |. Parser.LocalExtra.upTo 3 Parser.LocalExtra.space
            |. Parser.Advanced.symbol (Parser.Advanced.Token "-" (Parser.ExpectingSymbol "-"))
        , Parser.Advanced.succeed Plus
            |. Parser.Advanced.symbol (Parser.Advanced.Token "+" (Parser.ExpectingSymbol "+"))
        , Parser.Advanced.succeed Asterisk
            |. Parser.Advanced.symbol (Parser.Advanced.Token "*" (Parser.ExpectingSymbol "*"))
        ]


unorderedListItemBodyParser : Parser.Advanced.Parser String Parser.Problem ( Int, ListItem )
unorderedListItemBodyParser =
    Parser.Advanced.succeed (\bodyStartPos item -> ( bodyStartPos, item ))
        |. chompOneOrMore Char.LocalExtra.isSpaceOrTab
        |= Parser.Advanced.getCol
        |= RoughMarkdown.ListItem.parser


unorderedListEmptyItemParser : Parser.Advanced.Parser String Parser.Problem ( Int, ListItem )
unorderedListEmptyItemParser =
    Parser.Advanced.succeed (\bodyStartPos -> ( bodyStartPos, EmptyItem ))
        |= Parser.Advanced.getCol
        |. Parser.LocalExtra.lineEndOrEnd


getIntendedCodeItem : Int -> b -> Int -> ( Int, ListItem ) -> ( b, Int, ListItem )
getIntendedCodeItem markerStartPos listMarker markerEndPos ( bodyStartPos, item ) =
    let
        spaceNum : Int
        spaceNum =
            bodyStartPos - markerEndPos
    in
    if spaceNum <= 4 then
        ( listMarker, bodyStartPos - markerStartPos, item )

    else
        let
            intendedCodeItem : ListItem
            intendedCodeItem =
                case item of
                    TaskItem completion string ->
                        TaskItem completion (String.repeat (spaceNum - 1) " " ++ string)

                    PlainItem string ->
                        PlainItem (String.repeat (spaceNum - 1) " " ++ string)

                    EmptyItem ->
                        EmptyItem
        in
        ( listMarker, markerEndPos - markerStartPos + 1, intendedCodeItem )
