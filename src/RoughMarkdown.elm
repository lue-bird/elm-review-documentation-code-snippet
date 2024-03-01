module RoughMarkdown exposing
    ( Block(..)
    , ListItem(..), Task(..), ListSpacing(..)
    , foldl, parse
    )

{-| enough to make out all code blocks

@docs Block


### list items

@docs ListItem, Task, ListSpacing


## transforms

@docs foldl, parse

-}

import Char.LocalExtra
import Parser
import Parser.Advanced exposing ((|.), (|=), Step(..), andThen, chompIf, chompWhile, getChompedString, loop, map, oneOf, problem, succeed, symbol, token)
import Parser.LocalExtra
import Parser.Token
import RoughMarkdown.CodeBlock exposing (CodeBlock)
import RoughMarkdown.ListItem
import RoughMarkdown.OrderedList exposing (OrderedListMarker)
import RoughMarkdown.UnorderedList exposing (UnorderedListMarker)
import String exposing (repeat, trim)


type alias OpenListItem =
    { marker : UnorderedListMarker
    , body : String
    , task : Maybe Bool
    }


type alias CloseListItem =
    { task : Maybe Bool
    , body : List RawBlock
    }


type RawBlock
    = RawOpenBlockOrParagraph String
    | RawUnorderedListBlock Bool Int (List CloseListItem) OpenListItem
    | RawOrderedListBlock Bool Int OrderedListMarker Int (List (List RawBlock)) String
    | RawCodeBlock CodeBlock
    | RawIndentedCodeBlock String
    | RawBlankLine


{-| AST representing a parsed markdown chunk
-}
type Block
    = UnorderedList ListSpacing (List (ListItem Block))
    | OrderedList ListSpacing Int (List (List Block))
    | Paragraph String
    | CodeBlock { body : String, language : Maybe String }


{-| Based on the whitespace around lists, markdown will wrap each list item with a paragraph (if it's a `Loose` list) or it won't (if it's a `Tight` list).

<https://github.github.com/gfm/#lists>

> A list is loose if any of its constituent list items are separated by blank lines, or if any of its constituent list items directly contain two block-level elements with a blank line between them. Otherwise a list is tight. (The difference in HTML output is that paragraphs in a loose list are wrapped in <p> tags, while paragraphs in a tight list are not.)

-}
type ListSpacing
    = Loose
    | Tight


{-| The value for an unordered list item, which may contain a task.
-}
type ListItem children
    = ListItem Task (List children)


{-| A task (or no task), which may be contained in a ListItem.
-}
type Task
    = NoTask
    | IncompleteTask
    | CompletedTask


{-| Fold over all blocks and sub-blocks to yield a value.
-}
foldl : (Block -> folded -> folded) -> folded -> List Block -> folded
foldl function initialFolded list =
    case list of
        [] ->
            initialFolded

        block :: remainingBlocks ->
            case block of
                UnorderedList spacing blocks ->
                    foldl function
                        (function (UnorderedList spacing blocks) initialFolded)
                        ((blocks |> List.concatMap (\(ListItem _ children) -> children))
                            ++ remainingBlocks
                        )

                OrderedList spacing number blocks ->
                    foldl function (function (OrderedList spacing number blocks) initialFolded) (List.concat blocks ++ remainingBlocks)

                Paragraph text ->
                    foldl function (function (Paragraph text) initialFolded) remainingBlocks

                CodeBlock raw ->
                    foldl function (function (CodeBlock raw) initialFolded) remainingBlocks


rawBlockParser : Parser State
rawBlockParser =
    loop
        { linkReferenceDefinitions = []
        , rawBlocks = []
        }
        stepRawBlock
        |> andThen completeBlocks


parseAllInlines : State -> Result Parser.Problem (List Block)
parseAllInlines state =
    parseAllInlinesHelp state state.rawBlocks []


{-| Try parsing a markdown String into `RoughMarkdown.Block`s.

Often you'll want to render these `Block`s directly:

    render renderer markdown =
        markdown
            |> RoughMarkdown.Parser.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> RoughMarkdown.Renderer.render renderer ast)

    deadEndsToString deadEnds =
        deadEnds
            |> List.map RoughMarkdown.Parser.deadEndToString
            |> String.join "\n"

But you can also do a lot with the `Block`s before passing them through:

  - Transform the `Block`s ([example: make each heading one level deeper](https://github.com/dillonkearns/elm-markdown/blob/8fa879e72d33dec98d5cf95af2a8f8cf8c6d5d10/tests/BlockTransformTests.elm#L84-L133))
  - Use the blocks to gather metadata about the markdown document ([example: building a table of contents from `Block`s](https://ellie-app.com/cHB3fRSKVRha1))

-}
parse : String -> List Block
parse input =
    -- first parse the file as raw blocks
    case Parser.Advanced.run (rawBlockParser |. Parser.LocalExtra.endOfFile) input of
        Err _ ->
            [ input |> Paragraph ]

        Ok v ->
            -- then parse the inlines of each raw block
            case parseAllInlines v of
                Err e ->
                    Parser.Advanced.run (Parser.Advanced.problem e) ""
                        |> Result.withDefault [ input |> Paragraph ]

                Ok blocks ->
                    blocks
                        |> List.filterMap
                            (\block ->
                                case block of
                                    Paragraph "" ->
                                        Nothing

                                    notEmptyParagraph ->
                                        notEmptyParagraph |> Just
                            )


type alias Parser a =
    Parser.Advanced.Parser String Parser.Problem a


type InlineResult
    = EmptyBlock
    | ParsedBlock Block


parseInlines : LinkReferenceDefinitions -> RawBlock -> InlineResult
parseInlines linkReferences rawBlock =
    case rawBlock of
        RawOpenBlockOrParagraph string ->
            string
                |> Paragraph
                |> ParsedBlock

        RawUnorderedListBlock tight _ unparsedItems _ ->
            let
                parseItem : Maybe Bool -> List RawBlock -> ListItem Block
                parseItem rawBlockTask rawBlocks =
                    let
                        blocks : List Block
                        blocks =
                            case parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks } of
                                Ok parsedBlocks ->
                                    parsedBlocks

                                Err _ ->
                                    []

                        blocksTask : Task
                        blocksTask =
                            case rawBlockTask of
                                Just False ->
                                    IncompleteTask

                                Just True ->
                                    CompletedTask

                                Nothing ->
                                    NoTask
                    in
                    ListItem blocksTask blocks
            in
            unparsedItems
                |> List.map (\item -> parseItem item.task item.body)
                |> List.reverse
                |> UnorderedList (isTightBoolToListDisplay tight)
                |> ParsedBlock

        RawOrderedListBlock tight _ _ startingIndex unparsedItems _ ->
            let
                parseItem : List RawBlock -> List Block
                parseItem rawBlocks =
                    case parseAllInlines { linkReferenceDefinitions = linkReferences, rawBlocks = rawBlocks } of
                        Ok parsedBlocks ->
                            parsedBlocks

                        Err _ ->
                            []
            in
            unparsedItems
                |> List.map parseItem
                |> List.reverse
                |> OrderedList (isTightBoolToListDisplay tight) startingIndex
                |> ParsedBlock

        RawCodeBlock codeBlock ->
            CodeBlock codeBlock |> ParsedBlock

        RawBlankLine ->
            EmptyBlock

        RawIndentedCodeBlock codeBlockBody ->
            CodeBlock { body = codeBlockBody, language = Nothing }
                |> ParsedBlock


isTightBoolToListDisplay : Bool -> ListSpacing
isTightBoolToListDisplay isTight =
    if isTight then
        Tight

    else
        Loose


type alias LinkReferenceDefinitions =
    List ( String, { destination : String, title : Maybe String } )


type alias State =
    { linkReferenceDefinitions : LinkReferenceDefinitions
    , rawBlocks : List RawBlock
    }


parseAllInlinesHelp : State -> List RawBlock -> List Block -> Result Parser.Problem (List Block)
parseAllInlinesHelp state rawBlocks parsedBlocks =
    case rawBlocks of
        [] ->
            Ok parsedBlocks

        rawBlock :: rest ->
            case parseInlines state.linkReferenceDefinitions rawBlock of
                ParsedBlock newParsedBlock ->
                    parseAllInlinesHelp state rest (newParsedBlock :: parsedBlocks)

                EmptyBlock ->
                    -- ignore empty blocks
                    parseAllInlinesHelp state rest parsedBlocks


deadEndsToString : List (Parser.Advanced.DeadEnd String Parser.Problem) -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n"


{-| Turn a parsing problem into the default String representation.
-}
deadEndToString : Parser.Advanced.DeadEnd String Parser.Problem -> String
deadEndToString deadEnd =
    [ "Problem at row ", String.fromInt deadEnd.row, "\n", problemToString deadEnd.problem ] |> String.concat


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting string ->
            "Expecting " ++ string

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol string ->
            "Expecting symbol " ++ string

        Parser.ExpectingKeyword string ->
            "Expecting keyword " ++ string

        Parser.ExpectingEnd ->
            "Expecting keyword end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem problemDescription ->
            problemDescription

        Parser.BadRepeat ->
            "Bad repeat"


joinRawStringsWith : String -> String -> String -> String
joinRawStringsWith joinWith string1 string2 =
    case ( string1, string2 ) of
        ( "", s ) ->
            s

        ( s, "" ) ->
            s

        ( s1, s2 ) ->
            [ s1, joinWith, s2 ] |> String.concat


completeOrMergeBlocks : State -> RawBlock -> Parser State
completeOrMergeBlocks state newRawBlock =
    case ( newRawBlock, state.rawBlocks ) of
        ( RawCodeBlock block1, (RawCodeBlock block2) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    RawCodeBlock
                        { body = [ block2.body, "\n", block1.body ] |> String.concat
                        , language = Nothing
                        }
                        :: rest
                }

        ( RawIndentedCodeBlock block1, (RawIndentedCodeBlock block2) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    RawIndentedCodeBlock ([ block2, "\n", block1 ] |> String.concat)
                        :: rest
                }

        ( RawBlankLine, (RawIndentedCodeBlock block) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    RawIndentedCodeBlock (block ++ "\n\n")
                        :: rest
                }

        ( newToUnorderedListBlock, (RawUnorderedListBlock tight intended1 closeListItems2 openListItem2) :: rest ) ->
            case newToUnorderedListBlock of
                RawUnorderedListBlock _ intended2 _ openListItem1 ->
                    if openListItem2.marker == openListItem1.marker then
                        case Parser.Advanced.run rawBlockParser openListItem2.body of
                            Ok value ->
                                if List.member RawBlankLine value.rawBlocks then
                                    succeed
                                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                        , rawBlocks = RawUnorderedListBlock False intended2 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem1 :: rest
                                        }

                                else
                                    succeed
                                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                        , rawBlocks = RawUnorderedListBlock tight intended2 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem1 :: rest
                                        }

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                    else
                        case Parser.Advanced.run rawBlockParser openListItem2.body of
                            Ok value ->
                                let
                                    tight2 : Bool
                                    tight2 =
                                        if List.member RawBlankLine value.rawBlocks then
                                            False

                                        else
                                            tight
                                in
                                succeed
                                    { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                    , rawBlocks = newRawBlock :: RawUnorderedListBlock tight2 intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem1 :: rest
                                    }

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                RawOpenBlockOrParagraph body1 ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            RawUnorderedListBlock tight intended1 closeListItems2 { openListItem2 | body = joinRawStringsWith "\n" openListItem2.body body1 }
                                :: rest
                        }

                _ ->
                    case Parser.Advanced.run rawBlockParser openListItem2.body of
                        Ok value ->
                            let
                                tight2 : Bool
                                tight2 =
                                    if List.member RawBlankLine value.rawBlocks then
                                        False

                                    else
                                        tight
                            in
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = newRawBlock :: RawUnorderedListBlock tight2 intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem2 :: rest
                                }

                        Err e ->
                            problem (Parser.Problem (deadEndsToString e))

        -- OrderedListBlock Bool Int OrderedListMarker Int (List (List RawBlock)) String
        -- (\item -> OrderedListBlock True item.intended item.marker item.order [] item.body)
        ( _, (RawOrderedListBlock tight intended1 marker order closeListItems2 openListItem2) :: rest ) ->
            case newRawBlock of
                RawOrderedListBlock _ intended2 marker2 _ _ openListItem1 ->
                    if marker == marker2 then
                        case Parser.Advanced.run rawBlockParser openListItem2 of
                            Ok value ->
                                let
                                    tight2 : Bool
                                    tight2 =
                                        if List.member RawBlankLine value.rawBlocks then
                                            False

                                        else
                                            tight
                                in
                                succeed
                                    { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                    , rawBlocks = RawOrderedListBlock tight2 intended2 marker order (value.rawBlocks :: closeListItems2) openListItem1 :: rest
                                    }

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                    else
                        case Parser.Advanced.run rawBlockParser openListItem2 of
                            Ok value ->
                                let
                                    tight2 : Bool
                                    tight2 =
                                        if List.member RawBlankLine value.rawBlocks then
                                            False

                                        else
                                            tight
                                in
                                succeed
                                    { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                    , rawBlocks = newRawBlock :: RawOrderedListBlock tight2 intended1 marker order (value.rawBlocks :: closeListItems2) openListItem2 :: rest
                                    }

                            Err e ->
                                problem (Parser.Problem (deadEndsToString e))

                RawOpenBlockOrParagraph body1 ->
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions
                        , rawBlocks =
                            RawOrderedListBlock tight intended1 marker order closeListItems2 ([ openListItem2, "\n", body1 ] |> String.concat)
                                :: rest
                        }

                _ ->
                    case Parser.Advanced.run rawBlockParser openListItem2 of
                        Ok value ->
                            let
                                tight2 : Bool
                                tight2 =
                                    if List.member RawBlankLine value.rawBlocks then
                                        False

                                    else
                                        tight
                            in
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = newRawBlock :: RawOrderedListBlock tight2 intended1 marker order (value.rawBlocks :: closeListItems2) openListItem2 :: rest
                                }

                        Err e ->
                            problem (Parser.Problem (deadEndsToString e))

        ( RawOpenBlockOrParagraph body1, (RawOpenBlockOrParagraph body2) :: rest ) ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks =
                    RawOpenBlockOrParagraph (joinRawStringsWith "\n" body2 body1)
                        :: rest
                }

        ( _, RawBlankLine :: (RawOrderedListBlock tight intended1 marker order closeListItems2 openListItem2) :: rest ) ->
            case Parser.Advanced.run rawBlockParser openListItem2 of
                Ok value ->
                    case newRawBlock of
                        RawOrderedListBlock _ intended2 _ _ _ openListItem ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = RawOrderedListBlock False intended2 marker order (value.rawBlocks :: closeListItems2) openListItem :: rest
                                }

                        _ ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = newRawBlock :: RawBlankLine :: RawOrderedListBlock tight intended1 marker order (value.rawBlocks :: closeListItems2) openListItem2 :: rest
                                }

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        ( _, RawBlankLine :: (RawUnorderedListBlock tight intended1 closeListItems2 openListItem2) :: rest ) ->
            case Parser.Advanced.run rawBlockParser openListItem2.body of
                Ok value ->
                    case newRawBlock of
                        RawUnorderedListBlock _ _ _ openListItem ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = RawUnorderedListBlock False intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem :: rest
                                }

                        _ ->
                            succeed
                                { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                                , rawBlocks = newRawBlock :: RawBlankLine :: RawUnorderedListBlock tight intended1 ({ task = openListItem2.task, body = value.rawBlocks } :: closeListItems2) openListItem2 :: rest
                                }

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        _ ->
            succeed
                { linkReferenceDefinitions = state.linkReferenceDefinitions
                , rawBlocks = newRawBlock :: state.rawBlocks
                }


blankLine : Parser RawBlock
blankLine =
    Parser.Advanced.backtrackable (chompWhile Char.LocalExtra.isSpaceOrTab)
        |. Parser.LocalExtra.lineEnd
        |> map (\_ -> RawBlankLine)


stepRawBlock : State -> Parser (Step State State)
stepRawBlock revStatements =
    -- Some blocks can't immediately follow a body
    oneOf
        [ Parser.LocalExtra.endOfFile |> map (\_ -> Done revStatements)
        , case revStatements.rawBlocks of
            (RawOpenBlockOrParagraph _) :: _ ->
                mergeableBlockAfterOpenBlockOrParagraphParser
                    |> andThen (completeOrMergeBlocks revStatements)
                    |> map (\block -> Loop block)

            (RawUnorderedListBlock tight intended closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                ({ openListItem | body = joinRawStringsWith "\n" openListItem.body newString }
                                    |> RawUnorderedListBlock tight intended closeListItems
                                )
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                RawBlankLine
                                    :: ({ openListItem | body = joinRawStringsWith "" openListItem.body newString }
                                            |> RawUnorderedListBlock tight intended closeListItems
                                       )
                                    :: rest
                        }
                in
                oneOf
                    [ blankLine
                        |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStatements "\n")
                        |> map (\block -> Loop block)
                    , succeed identity
                        |. Parser.Advanced.symbol (Parser.Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                        |= getChompedString Parser.LocalExtra.chompUntilLineEndOrEnd
                        |. Parser.LocalExtra.lineEndOrEnd
                        |> map (completeOrMergeUnorderedListBlock revStatements)
                        |> map (\block -> Loop block)
                    , mergeableBlockAfterList
                        |> andThen (completeOrMergeBlocks revStatements)
                        |> map (\block -> Loop block)
                    ]

            RawBlankLine :: (RawUnorderedListBlock tight intended closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                ({ openListItem | body = joinRawStringsWith "\n" openListItem.body newString }
                                    |> RawUnorderedListBlock tight intended closeListItems
                                )
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                RawBlankLine
                                    :: ({ openListItem | body = joinRawStringsWith "" openListItem.body newString }
                                            |> RawUnorderedListBlock tight intended closeListItems
                                       )
                                    :: rest
                        }
                in
                case trim openListItem.body of
                    "" ->
                        mergeableBlockNotAfterOpenBlockOrParagraphParser
                            |> andThen (completeOrMergeBlocks revStatements)
                            |> map (\block -> Loop block)

                    _ ->
                        oneOf
                            [ blankLine
                                |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStatements "\n")
                                |> map (\block -> Loop block)
                            , succeed identity
                                |. Parser.Advanced.symbol (Parser.Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                                |= getChompedString Parser.LocalExtra.chompUntilLineEndOrEnd
                                |. Parser.LocalExtra.lineEndOrEnd
                                |> map (completeOrMergeUnorderedListBlock revStatements)
                                |> map (\block -> Loop block)
                            , mergeableBlockNotAfterOpenBlockOrParagraphParser
                                |> andThen (completeOrMergeBlocks revStatements)
                                |> map (\block -> Loop block)
                            ]

            (RawOrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                RawOrderedListBlock tight intended marker order closeListItems ([ openListItem, "\n", newString ] |> String.concat)
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                RawBlankLine
                                    :: RawOrderedListBlock tight intended marker order closeListItems ([ openListItem, "\n", newString ] |> String.concat)
                                    :: rest
                        }
                in
                oneOf
                    [ blankLine
                        |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStatements "\n")
                        |> map (\block -> Loop block)
                    , succeed identity
                        |. Parser.Advanced.symbol (Parser.Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                        |= getChompedString Parser.LocalExtra.chompUntilLineEndOrEnd
                        |. Parser.LocalExtra.lineEndOrEnd
                        |> map (completeOrMergeUnorderedListBlock revStatements)
                        |> map (\block -> Loop block)
                    , mergeableBlockAfterList
                        |> andThen (completeOrMergeBlocks revStatements)
                        |> map (\block -> Loop block)
                    ]

            RawBlankLine :: (RawOrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
                let
                    completeOrMergeUnorderedListBlock : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlock state newString =
                        { state
                            | rawBlocks =
                                RawOrderedListBlock tight intended marker order closeListItems ([ openListItem, "\n", newString ] |> String.concat)
                                    :: rest
                        }

                    completeOrMergeUnorderedListBlockBlankLine : { a | rawBlocks : List RawBlock } -> String.String -> { a | rawBlocks : List RawBlock }
                    completeOrMergeUnorderedListBlockBlankLine state newString =
                        { state
                            | rawBlocks =
                                RawBlankLine
                                    :: RawOrderedListBlock tight intended marker order closeListItems ([ openListItem, "\n", newString ] |> String.concat)
                                    :: rest
                        }
                in
                case trim openListItem of
                    "" ->
                        mergeableBlockNotAfterOpenBlockOrParagraphParser
                            |> andThen (completeOrMergeBlocks revStatements)
                            |> map (\block -> Loop block)

                    _ ->
                        oneOf
                            [ blankLine
                                |> map (\_ -> completeOrMergeUnorderedListBlockBlankLine revStatements "\n")
                                |> map (\block -> Loop block)
                            , succeed identity
                                |. Parser.Advanced.symbol (Parser.Advanced.Token (repeat intended " ") (Parser.ExpectingSymbol "Indentation"))
                                |= getChompedString Parser.LocalExtra.chompUntilLineEndOrEnd
                                |. Parser.LocalExtra.lineEndOrEnd
                                |> map (completeOrMergeUnorderedListBlock revStatements)
                                |> map (\block -> Loop block)
                            , mergeableBlockNotAfterOpenBlockOrParagraphParser
                                |> andThen (completeOrMergeBlocks revStatements)
                                |> map (\block -> Loop block)
                            ]

            _ ->
                mergeableBlockNotAfterOpenBlockOrParagraphParser
                    |> andThen (completeOrMergeBlocks revStatements)
                    |> map (\block -> Loop block)
        , openBlockOrParagraphParser
            |> andThen (completeOrMergeBlocks revStatements)
            |> map (\block -> Loop block)
        ]


openBlockOrParagraphParser : Parser RawBlock
openBlockOrParagraphParser =
    innerParagraphParser
        |. Parser.LocalExtra.lineEndOrEnd


innerParagraphParser : Parser RawBlock
innerParagraphParser =
    Parser.LocalExtra.chompUntilLineEndOrEnd
        |> Parser.Advanced.mapChompedString
            (\rawLine _ -> rawLine |> RawOpenBlockOrParagraph)



-- RAW BLOCK PARSER


unorderedListBlock : Bool -> Parser RawBlock
unorderedListBlock previousWasBody =
    let
        parseListItem : a -> RoughMarkdown.ListItem.ListItem -> { body : String.String, task : Maybe Bool, marker : a }
        parseListItem listMarker unparsedListItem =
            case unparsedListItem of
                RoughMarkdown.ListItem.TaskItem completion body ->
                    { body = body
                    , task =
                        (case completion of
                            RoughMarkdown.ListItem.Complete ->
                                True

                            RoughMarkdown.ListItem.Incomplete ->
                                False
                        )
                            |> Just
                    , marker = listMarker
                    }

                RoughMarkdown.ListItem.PlainItem body ->
                    { body = body
                    , task = Nothing
                    , marker = listMarker
                    }

                RoughMarkdown.ListItem.EmptyItem ->
                    { body = "" --++ Debug.toString (Parser.Advanced.run getIndent "     1   2")
                    , task = Nothing
                    , marker = listMarker
                    }
    in
    RoughMarkdown.UnorderedList.parser previousWasBody
        |> map
            (\( listMarker, intended, unparsedListItem ) ->
                RawUnorderedListBlock True
                    intended
                    []
                    (parseListItem listMarker unparsedListItem)
            )


orderedListBlock : Bool -> Parser RawBlock
orderedListBlock previousWasBody =
    RoughMarkdown.OrderedList.parser previousWasBody
        |> map (\item -> RawOrderedListBlock True item.intended item.marker item.order [] item.body)



-- Note [Static Parser Structure]
--
-- For performance reasons, it is VERY IMPORTANT that `mergeableBlockAfterOpenBlockOrParagraphParser` and `mergeableBlockNotAfterOpenBlockOrParagraphParser`
-- defined as `var` in javascript (and not as a function taking any, even zero, arguments).
--
-- A `var` is defined once, then re-used for every raw block we parse. If they were functions, the parser
-- structure would need to be re-built for every raw
-- Because there are lists involved (in the `oneOf`s), that is expensive.
--
-- All my attempts so far to "DRY" this code below cause a degradation in performance.


{-| HTML parsing is intentionally strict in `dillonkearns/elm-markdown`. Paragraphs are supposed to be forgiving.
This function checks to see if something might be an autolink that could be confused with an HTML block because
the line starts with `<`. But it's slightly more lenient, so that things like `<>` that aren't actually parsed as
autolinks are still parsed as paragraphs.
-}
parseAsParagraphInsteadOfHtmlBlock : Parser RawBlock
parseAsParagraphInsteadOfHtmlBlock =
    -- ^<[A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*>
    token (Parser.Advanced.Token "<" (Parser.Expecting "<"))
        |. thisIsDefinitelyNotAnHtmlTag
        |. Parser.LocalExtra.chompUntilLineEndOrEnd
        |. Parser.LocalExtra.lineEndOrEnd
        |> Parser.Advanced.mapChompedString (\rawLine _ -> rawLine |> RawOpenBlockOrParagraph)
        |> Parser.Advanced.backtrackable


thisIsDefinitelyNotAnHtmlTag : Parser ()
thisIsDefinitelyNotAnHtmlTag =
    oneOf
        [ token (Parser.Advanced.Token " " (Parser.Expecting " "))
        , token (Parser.Advanced.Token ">" (Parser.Expecting ">"))
        , chompIf Char.isAlpha (Parser.Expecting "Alpha")
            |. chompWhile (\c -> (c |> Char.isAlphaNum) || (c |> Char.LocalExtra.isHyphen))
            |. oneOf
                [ token (Parser.Advanced.Token ":" (Parser.Expecting ":"))
                , token (Parser.Advanced.Token "@" (Parser.Expecting "@"))
                , token (Parser.Advanced.Token "\\" (Parser.Expecting "\\"))
                , token (Parser.Advanced.Token "+" (Parser.Expecting "+"))
                , token (Parser.Advanced.Token "." (Parser.Expecting "."))
                ]
        ]


mergeableBlockAfterOpenBlockOrParagraphParser : Parser RawBlock
mergeableBlockAfterOpenBlockOrParagraphParser =
    oneOf
        [ parseAsParagraphInsteadOfHtmlBlock
        , blankLine
        , RoughMarkdown.CodeBlock.parser |> Parser.Advanced.backtrackable |> map RawCodeBlock
        , unorderedListBlock True

        -- NOTE: the ordered list block changes its parsing rules when it's right after a Body
        , orderedListBlock True
        ]


mergeableBlockAfterList : Parser RawBlock
mergeableBlockAfterList =
    oneOf
        [ parseAsParagraphInsteadOfHtmlBlock
        , blankLine
        , RoughMarkdown.CodeBlock.parser |> Parser.Advanced.backtrackable |> map RawCodeBlock

        -- NOTE: both the unordered and ordered lists block changes its parsing rules when it's right after a Body
        , unorderedListBlock False
        , orderedListBlock False
        ]


mergeableBlockNotAfterOpenBlockOrParagraphParser : Parser RawBlock
mergeableBlockNotAfterOpenBlockOrParagraphParser =
    oneOf
        [ parseAsParagraphInsteadOfHtmlBlock
        , blankLine
        , RoughMarkdown.CodeBlock.parser |> Parser.Advanced.backtrackable |> map RawCodeBlock
        , indentedCodeBlock

        -- NOTE: both the unordered and ordered lists block changes its parsing rules when it's right after a Body
        , unorderedListBlock False
        , orderedListBlock False
        ]


indentedCodeBlock : Parser RawBlock
indentedCodeBlock =
    succeed RawIndentedCodeBlock
        |. exactlyFourSpaces
        |= getChompedString Parser.LocalExtra.chompUntilLineEndOrEnd
        |. Parser.LocalExtra.lineEndOrEnd


exactlyFourSpaces : Parser ()
exactlyFourSpaces =
    oneOf
        [ symbol Parser.Token.tab
        , Parser.Advanced.backtrackable (symbol Parser.Token.space)
            |. oneOf
                [ Parser.Advanced.symbol (Parser.Advanced.Token "   " (Parser.ExpectingSymbol "Indentation"))
                , Parser.Advanced.symbol (Parser.Advanced.Token " \t" (Parser.ExpectingSymbol "Indentation"))
                , Parser.Advanced.symbol (Parser.Advanced.Token "  \t" (Parser.ExpectingSymbol "Indentation"))
                ]
        ]


completeBlocks :
    State
    -> Parser State --Result Parser.Problem (List Block)
completeBlocks state =
    case state.rawBlocks of
        (RawUnorderedListBlock tight intended closeListItems openListItem) :: rest ->
            case Parser.Advanced.run rawBlockParser openListItem.body of
                Ok value ->
                    let
                        tight2 : Bool
                        tight2 =
                            if List.member RawBlankLine value.rawBlocks then
                                False

                            else
                                tight
                    in
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = RawUnorderedListBlock tight2 intended ({ task = openListItem.task, body = value.rawBlocks } :: closeListItems) openListItem :: rest
                        }

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        RawBlankLine :: (RawUnorderedListBlock tight intended closeListItems openListItem) :: rest ->
            case Parser.Advanced.run rawBlockParser openListItem.body of
                Ok value ->
                    let
                        tight2 : Bool
                        tight2 =
                            if List.member RawBlankLine value.rawBlocks then
                                False

                            else
                                tight
                    in
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = RawUnorderedListBlock tight2 intended ({ task = openListItem.task, body = value.rawBlocks } :: closeListItems) openListItem :: rest
                        }

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        (RawOrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
            case Parser.Advanced.run rawBlockParser openListItem of
                Ok value ->
                    let
                        tight2 : Bool
                        tight2 =
                            if List.member RawBlankLine value.rawBlocks then
                                False

                            else
                                tight
                    in
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = RawOrderedListBlock tight2 intended marker order (value.rawBlocks :: closeListItems) openListItem :: rest
                        }

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        RawBlankLine :: (RawOrderedListBlock tight intended marker order closeListItems openListItem) :: rest ->
            case Parser.Advanced.run rawBlockParser openListItem of
                Ok value ->
                    let
                        tight2 : Bool
                        tight2 =
                            if List.member RawBlankLine value.rawBlocks then
                                False

                            else
                                tight
                    in
                    succeed
                        { linkReferenceDefinitions = state.linkReferenceDefinitions ++ value.linkReferenceDefinitions
                        , rawBlocks = RawOrderedListBlock tight2 intended marker order (value.rawBlocks :: closeListItems) openListItem :: rest
                        }

                Err e ->
                    problem (Parser.Problem (deadEndsToString e))

        _ ->
            succeed state
