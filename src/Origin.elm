module Origin exposing (determine)

import Elm.Syntax.ModuleName
import FastDict
import FastDict.LocalExtra
import Imports exposing (Imports)
import Set


determine : Imports -> (( Elm.Syntax.ModuleName.ModuleName, String ) -> Elm.Syntax.ModuleName.ModuleName)
determine imports =
    \reference ->
        case reference of
            ( [], "List" ) ->
                -- the only reference that has no origin, so e.g. List.List is a compiler error.
                []

            ( qualification, unqualifiedName ) ->
                case imports |> FastDict.get qualification of
                    Just _ ->
                        qualification

                    Nothing ->
                        let
                            maybeOriginByAlias : Maybe Elm.Syntax.ModuleName.ModuleName
                            maybeOriginByAlias =
                                imports
                                    |> FastDict.LocalExtra.firstJustMap
                                        (\importModuleName import_ ->
                                            case import_.alias of
                                                Nothing ->
                                                    Nothing

                                                Just alias ->
                                                    if qualification == [ alias ] then
                                                        importModuleName |> Just

                                                    else
                                                        Nothing
                                        )
                        in
                        case maybeOriginByAlias of
                            Just aliasOriginModuleName ->
                                aliasOriginModuleName

                            Nothing ->
                                case qualification of
                                    [] ->
                                        let
                                            maybeOriginByExpose : Maybe Elm.Syntax.ModuleName.ModuleName
                                            maybeOriginByExpose =
                                                imports
                                                    |> FastDict.LocalExtra.firstJustMap
                                                        (\importModuleName import_ ->
                                                            if import_.exposed |> Set.member unqualifiedName then
                                                                importModuleName |> Just

                                                            else
                                                                Nothing
                                                        )
                                        in
                                        case maybeOriginByExpose of
                                            Just moduleName ->
                                                moduleName

                                            Nothing ->
                                                -- defined branch-locally (pattern variable or let declared)
                                                []

                                    moduleNamePart0 :: moduleNamePart1 ->
                                        moduleNamePart0 :: moduleNamePart1
