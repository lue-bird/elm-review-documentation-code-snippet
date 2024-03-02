module ElmSyntaxParse exposing (expression, importsAndDeclarations, type_)

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node
import Elm.Syntax.TypeAnnotation
import List.LocalExtra


importsAndDeclarationsDiscardingModuleHeader : String -> Maybe { imports : List Import, declarations : List Declaration }
importsAndDeclarationsDiscardingModuleHeader =
    \rawSourceCode ->
        -- inserts and removes a dummy declaration to account for 0 declarations
        case rawSourceCode ++ "\na = a" |> Elm.Parser.parseToFile of
            Ok file ->
                { imports = file.imports |> List.map Elm.Syntax.Node.value
                , declarations = file.declarations |> List.LocalExtra.removeLast |> List.map Elm.Syntax.Node.value
                }
                    |> Just

            Err _ ->
                Nothing


{-| Assumes `elm-format`ed code
-}
importsAndDeclarations : String -> Maybe { imports : List Import, declarations : List Declaration }
importsAndDeclarations =
    \rawSourceCode ->
        case rawSourceCode |> importsAndDeclarationsDiscardingModuleHeader of
            Just importsAndDeclarationsParsed ->
                importsAndDeclarationsParsed |> Just

            Nothing ->
                rawSourceCode |> importsAndDeclarationsWithoutModuleHeader


importsAndDeclarationsWithoutModuleHeader : String -> Maybe { imports : List Import, declarations : List Declaration }
importsAndDeclarationsWithoutModuleHeader =
    \rawSourceCode ->
        "module A exposing (..)\n" ++ rawSourceCode |> importsAndDeclarationsDiscardingModuleHeader


expression : String -> Maybe Expression
expression =
    \rawSourceCode ->
        let
            moduleSourceCode : String
            moduleSourceCode =
                "module A exposing (..)\na =\n"
                    ++ (rawSourceCode
                            |> String.split "\n"
                            |> List.map (\line -> "    " ++ line)
                            |> String.join "\n"
                       )
        in
        case moduleSourceCode |> Elm.Parser.parseToFile of
            Ok fileWithoutHeaderAndFnDeclarationHeader ->
                case fileWithoutHeaderAndFnDeclarationHeader.declarations of
                    [ Elm.Syntax.Node.Node _ (Elm.Syntax.Declaration.FunctionDeclaration valueDeclaration) ] ->
                        valueDeclaration.declaration |> Elm.Syntax.Node.value |> .expression |> Elm.Syntax.Node.value |> Just

                    _ ->
                        Nothing

            Err _ ->
                Nothing


type_ : String -> Maybe Elm.Syntax.TypeAnnotation.TypeAnnotation
type_ =
    \rawSourceCode ->
        let
            moduleSourceCode : String
            moduleSourceCode =
                "module A exposing (..)\nport a :\n"
                    ++ (rawSourceCode
                            |> String.split "\n"
                            |> List.map (\line -> "    " ++ line)
                            |> String.join "\n"
                       )
        in
        case moduleSourceCode |> Elm.Parser.parseToFile of
            Ok fileWithoutHeaderAndPortInsteadOfFunction ->
                case fileWithoutHeaderAndPortInsteadOfFunction.declarations of
                    [ Elm.Syntax.Node.Node _ (Elm.Syntax.Declaration.PortDeclaration portDeclaration) ] ->
                        portDeclaration.typeAnnotation |> Elm.Syntax.Node.value |> Just

                    _ ->
                        Nothing

            Err _ ->
                Nothing
