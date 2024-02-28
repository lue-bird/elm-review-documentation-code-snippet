module Char.LocalExtra exposing (isLineEnd, isSpaceOrTab)

{-| -}


{-| Line ending as defined in the GFM spec
Note that a line ending can also be a carriage return
followed by a newline.
-}
isLineEnd : Char -> Bool
isLineEnd char =
    case char of
        '\n' ->
            -- Newline
            True

        '\u{000D}' ->
            -- Carriage return
            True

        _ ->
            False


isSpaceOrTab : Char -> Bool
isSpaceOrTab char =
    case char of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False
