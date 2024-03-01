module Pattern.LocalExtra exposing (fullyQualify, nodeReferences, references)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Imports exposing (Imports)
import List.LocalExtra
import Origin
import Set exposing (Set)


fullyQualify : Imports -> (Pattern -> Pattern)
fullyQualify resources =
    \pattern ->
        pattern
            |> map
                (\innerPattern ->
                    case innerPattern of
                        Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                            Elm.Syntax.Pattern.NamedPattern
                                { name = fullyQualified.name
                                , moduleName =
                                    ( fullyQualified.moduleName, fullyQualified.name )
                                        |> Origin.determine resources
                                }
                                arguments

                        otherPattern ->
                            otherPattern
                )


{-| Map it, then all its sub-patterns, all the way down
-}
map : (Pattern -> Pattern) -> (Pattern -> Pattern)
map patternChange =
    let
        step : Node Pattern -> Node Pattern
        step =
            Elm.Syntax.Node.map (\stepPattern -> stepPattern |> map patternChange)
    in
    -- IGNORE TCO
    \pattern ->
        case pattern |> patternChange of
            Elm.Syntax.Pattern.AllPattern ->
                Elm.Syntax.Pattern.AllPattern

            Elm.Syntax.Pattern.UnitPattern ->
                Elm.Syntax.Pattern.UnitPattern

            Elm.Syntax.Pattern.CharPattern char ->
                Elm.Syntax.Pattern.CharPattern char

            Elm.Syntax.Pattern.StringPattern string ->
                Elm.Syntax.Pattern.StringPattern string

            Elm.Syntax.Pattern.IntPattern int ->
                Elm.Syntax.Pattern.IntPattern int

            Elm.Syntax.Pattern.HexPattern int ->
                Elm.Syntax.Pattern.HexPattern int

            Elm.Syntax.Pattern.FloatPattern float ->
                Elm.Syntax.Pattern.FloatPattern float

            Elm.Syntax.Pattern.VarPattern name ->
                Elm.Syntax.Pattern.VarPattern name

            Elm.Syntax.Pattern.RecordPattern fieldNames ->
                Elm.Syntax.Pattern.RecordPattern fieldNames

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                Elm.Syntax.Pattern.ParenthesizedPattern (inParens |> step)

            Elm.Syntax.Pattern.AsPattern aliased name ->
                Elm.Syntax.Pattern.AsPattern (aliased |> step) name

            Elm.Syntax.Pattern.UnConsPattern head tail ->
                Elm.Syntax.Pattern.UnConsPattern (head |> step) (tail |> step)

            Elm.Syntax.Pattern.TuplePattern parts ->
                Elm.Syntax.Pattern.TuplePattern (parts |> List.map step)

            Elm.Syntax.Pattern.ListPattern elements ->
                Elm.Syntax.Pattern.ListPattern (elements |> List.map step)

            Elm.Syntax.Pattern.NamedPattern qualified arguments ->
                Elm.Syntax.Pattern.NamedPattern qualified (arguments |> List.map step)


references : Pattern -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
references =
    -- IGNORE TCO
    \pattern ->
        case pattern of
            Elm.Syntax.Pattern.AllPattern ->
                Set.empty

            Elm.Syntax.Pattern.UnitPattern ->
                Set.empty

            Elm.Syntax.Pattern.CharPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.StringPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.IntPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.HexPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.FloatPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.VarPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.RecordPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                inParens |> nodeReferences

            Elm.Syntax.Pattern.AsPattern aliased _ ->
                aliased |> nodeReferences

            Elm.Syntax.Pattern.UnConsPattern head tail ->
                Set.union (tail |> nodeReferences) (head |> nodeReferences)

            Elm.Syntax.Pattern.TuplePattern parts ->
                parts |> List.LocalExtra.setUnionMap nodeReferences

            Elm.Syntax.Pattern.ListPattern elements ->
                elements |> List.LocalExtra.setUnionMap nodeReferences

            Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                arguments
                    |> List.LocalExtra.setUnionMap nodeReferences
                    |> Set.insert ( fullyQualified.moduleName, fullyQualified.name )


nodeReferences : Node Pattern -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
nodeReferences =
    \(Node _ innerPattern) -> innerPattern |> references
