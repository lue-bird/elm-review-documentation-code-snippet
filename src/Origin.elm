module Origin exposing (determine)

import Elm.Syntax.ModuleName
import FastDict exposing (Dict)
import Imports exposing (Imports)
import Set


determine : Imports -> (( Elm.Syntax.ModuleName.ModuleName, String ) -> Elm.Syntax.ModuleName.ModuleName)
determine imports =
    \( qualification, unqualifiedName ) ->
        case imports |> FastDict.get qualification of
            Just _ ->
                qualification

            Nothing ->
                case
                    imports
                        |> dictFirstJustMap
                            (\importModuleName import_ ->
                                if (import_.alias |> Maybe.map List.singleton) == Just qualification then
                                    importModuleName |> Just

                                else
                                    Nothing
                            )
                of
                    Just aliasOriginModuleName ->
                        aliasOriginModuleName

                    Nothing ->
                        imports
                            |> dictFirstJustMap
                                (\importModuleName import_ ->
                                    if import_.exposed |> Set.member unqualifiedName then
                                        importModuleName |> Just

                                    else
                                        Nothing
                                )
                            |> Maybe.withDefault qualification


dictFirstJustMap : (key -> value -> Maybe mapped) -> Dict key value -> Maybe mapped
dictFirstJustMap mapToMaybe =
    \dict ->
        dict
            |> FastDict.foldl
                (\importModuleName import_ soFar ->
                    case soFar of
                        Just alreadyFound ->
                            alreadyFound |> Just

                        Nothing ->
                            import_ |> mapToMaybe importModuleName
                )
                Nothing
