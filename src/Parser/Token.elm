module Parser.Token exposing (backtick, carriageReturn, closingParen, dot, newline, space, tab, tilde)

import Parser
import Parser.Advanced exposing (Token(..))


tab : Token Parser.Problem
tab =
    Token "\t" (Parser.Expecting "a tab")


space : Token Parser.Problem
space =
    Token " " (Parser.Expecting "a space")


newline : Token Parser.Problem
newline =
    Token "\n" (Parser.Expecting "a newline")


carriageReturn : Token Parser.Problem
carriageReturn =
    Token "\u{000D}" (Parser.Expecting "a carriage return")


dot : Token Parser.Problem
dot =
    Token "." (Parser.Expecting "a `.`")


closingParen : Token Parser.Problem
closingParen =
    Token ")" (Parser.Expecting "a `)`")


tilde : Token Parser.Problem
tilde =
    Token "~" (Parser.Expecting "a `~`")


backtick : Token Parser.Problem
backtick =
    Token "`" (Parser.Expecting "a '`'")
