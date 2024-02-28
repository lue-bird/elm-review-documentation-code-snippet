module RoughMarkdown.CodeBlock exposing (CodeBlock, Fence, FenceCharacter(..), FenceCharacterConfig, Parser, parser)

import Char.LocalExtra
import Parser
import Parser.Advanced exposing ((|.), (|=))
import Parser.LocalExtra
import Parser.Token


type alias Parser a =
    Parser.Advanced.Parser String Parser.Problem a


type alias CodeBlock =
    { language : Maybe String
    , body : String
    }


type alias Fence =
    { character : FenceCharacterConfig
    , length : Int
    , indented : Int
    }


type FenceCharacter
    = Backtick
    | Tilde


type alias FenceCharacterConfig =
    { kind : FenceCharacter
    , char : Char
    , token : Parser.Advanced.Token Parser.Problem
    }


parser : Parser CodeBlock
parser =
    openingFence
        |> Parser.Advanced.andThen
            (\fence ->
                Parser.Advanced.succeed (\language body -> { language = language, body = body })
                    |= infoString fence.character
                    |. Parser.LocalExtra.lineEndOrEnd
                    |= remainingBlock fence
            )


fenceOfAtLeast : Int -> FenceCharacterConfig -> Parser ( FenceCharacterConfig, Int )
fenceOfAtLeast minLength fenceCharacter =
    let
        -- Chains the token parsers together
        -- End up with succeed () |. token Token.tilde |. ... for minLength
        builtTokens : Parser.Advanced.Parser c Parser.Problem ()
        builtTokens =
            Parser.Advanced.token fenceCharacter.token
                |> List.repeat minLength
                |> List.foldl (\t p -> p |. t) (Parser.Advanced.succeed ())
    in
    builtTokens
        -- As long as we have minLength tokens, the rest are ok
        |. Parser.Advanced.chompWhile ((==) fenceCharacter.char)
        |> Parser.Advanced.mapChompedString (\str _ -> ( fenceCharacter, String.length str ))



--


openingFence : Parser Fence
openingFence =
    Parser.Advanced.succeed
        (\indent ( character, length ) ->
            { character = character, length = length, indented = indent }
        )
        |. Parser.LocalExtra.upToThreeSpaces
        -- Indentation
        |= (Parser.Advanced.getCol |> Parser.Advanced.andThen colToIndentation)
        |= Parser.Advanced.oneOf
            [ fenceOfAtLeast 3 backtick
            , fenceOfAtLeast 3 tilde
            ]



-- Parses the opening fence and with it information about
-- indentation, and what the closing fence should look like.


backtick : { kind : FenceCharacter, char : Char, token : Parser.Advanced.Token Parser.Problem }
backtick =
    { kind = Backtick, char = '`', token = Parser.Token.backtick }



-- In this case max three, as that's the max the opening fence can be indented.
-- getCol always starts from 1, so 1 needs to be subtracted.


tilde : { kind : FenceCharacter, char : Char, token : Parser.Advanced.Token Parser.Problem }
tilde =
    { kind = Tilde, char = '~', token = Parser.Token.tilde }



-- Parses the closing fence, making sure it is the right length


colToIndentation : Int -> Parser Int
colToIndentation int =
    case int of
        1 ->
            Parser.Advanced.succeed 0

        2 ->
            Parser.Advanced.succeed 1

        3 ->
            Parser.Advanced.succeed 2

        4 ->
            Parser.Advanced.succeed 3

        _ ->
            Parser.Advanced.problem (Parser.Expecting "Fenced code blocks should be indented no more than 3 spaces")



{- Code fence (from GFM):
   > sequence of at least three consecutive backtick characters (`) or tildes (~).
   > (Tildes and backticks cannot be mixed.)
-}


infoString : FenceCharacterConfig -> Parser (Maybe String)
infoString fenceCharacter =
    let
        toInfoString : String -> b -> Maybe String
        toInfoString str _ =
            case String.trim str of
                "" ->
                    Nothing

                trimmed ->
                    Just trimmed
    in
    case fenceCharacter.kind of
        Backtick ->
            Parser.Advanced.chompWhile (\c -> c /= '`' && not (Char.LocalExtra.isLineEnd c))
                |> Parser.Advanced.mapChompedString toInfoString

        Tilde ->
            Parser.Advanced.chompWhile (\c -> not (c |> Char.LocalExtra.isLineEnd))
                |> Parser.Advanced.mapChompedString toInfoString



{- Info string
   > The line with the opening code fence may optionally be followed by text.
   > This is trimmed of leading and trailing whitespace.
   > If coming after a ` fence, it must not contain any ` characters.
-}


remainingBlock : Fence -> Parser String
remainingBlock fence =
    Parser.Advanced.loop ( fence, "" ) remainingBlockHelp



{- Body
   > If the leading code fence is indented N spaces, then up to N spaces of
   > indentation are removed from each line of the content (if present).
-}


remainingBlockHelp : ( Fence, String ) -> Parser (Parser.Advanced.Step ( Fence, String ) String)
remainingBlockHelp ( fence, body ) =
    Parser.Advanced.oneOf
        -- End of the string
        [ Parser.Advanced.succeed (Parser.Advanced.Done body)
            |. Parser.Advanced.end Parser.ExpectingEnd

        -- End of the line, chomps up the line ending
        , Parser.LocalExtra.lineEnd
            |> Parser.Advanced.mapChompedString
                (\lineEnd _ -> Parser.Advanced.Loop ( fence, body ++ lineEnd ))

        -- Closing fence
        , Parser.Advanced.succeed (Parser.Advanced.Done body)
            |. closingFence fence.length fence.character
            |> Parser.Advanced.backtrackable

        -- A code line. `codeBlockLine` returns the offset after any extra indentation
        -- is collapsed. The line is then sliced and added to the body. This is the
        -- equivalent to `mapChompedString` with a custom start value.
        , Parser.Advanced.succeed
            (\start end source ->
                Parser.Advanced.Loop ( fence, body ++ String.slice start end source )
            )
            |= codeBlockLine fence.indented
            |= Parser.Advanced.getOffset
            |= Parser.Advanced.getSource
        ]


closingFence : Int -> FenceCharacterConfig -> Parser ()
closingFence minLength fenceCharacter =
    Parser.Advanced.succeed ()
        |. Parser.LocalExtra.upToThreeSpaces
        |. fenceOfAtLeast minLength fenceCharacter
        |. Parser.Advanced.chompWhile (\c -> c == ' ')
        |. Parser.LocalExtra.lineEndOrEnd


codeBlockLine : Int -> Parser Int
codeBlockLine indented =
    Parser.Advanced.succeed identity
        |. Parser.LocalExtra.upTo indented Parser.LocalExtra.space
        |= Parser.Advanced.getOffset
        |. Parser.LocalExtra.chompUntilLineEndOrEnd
        |. Parser.LocalExtra.lineEndOrEnd
