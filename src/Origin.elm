module Origin exposing (determine)

import Elm.Syntax.ModuleName
import FastDict exposing (Dict)
import FastDict.LocalExtra
import Imports exposing (Imports)
import Set


determine : Imports -> (( Elm.Syntax.ModuleName.ModuleName, String ) -> Elm.Syntax.ModuleName.ModuleName)
determine imports =
    \( qualification, unqualifiedName ) ->
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
                        imports
                            |> FastDict.LocalExtra.firstJustMap
                                (\importModuleName import_ ->
                                    if import_.exposed |> Set.member unqualifiedName then
                                        importModuleName |> Just

                                    else
                                        Nothing
                                )
                            |> Maybe.withDefault qualification
