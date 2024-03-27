module Origin exposing (determine)

import Elm.Syntax.ModuleName
import FastDict
import FastDict.LocalExtra
import Imports exposing (Imports)
import Set


determine : Imports -> (( Elm.Syntax.ModuleName.ModuleName, String ) -> Maybe Elm.Syntax.ModuleName.ModuleName)
determine imports =
    \reference ->
        case reference of
            ( [], "List" ) ->
                -- the only reference that has no origin, so e.g. List.List is a compiler error.
                [] |> Just

            ( qualification, unqualifiedName ) ->
                case imports |> FastDict.get qualification of
                    Just _ ->
                        qualification |> Just

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
                                aliasOriginModuleName |> Just

                            Nothing ->
                                case qualification of
                                    [] ->
                                        imports
                                            |> FastDict.LocalExtra.firstJustMap
                                                (\importModuleName import_ ->
                                                    if import_.exposed |> Set.member unqualifiedName then
                                                        importModuleName |> Just

                                                    else
                                                        Nothing
                                                )

                                    _ :: _ ->
                                        Nothing
