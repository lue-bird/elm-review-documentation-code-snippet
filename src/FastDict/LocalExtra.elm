module FastDict.LocalExtra exposing (firstJustMap)

import FastDict exposing (Dict)


firstJustMap : (key -> value -> Maybe mapped) -> Dict key value -> Maybe mapped
firstJustMap mapToMaybe =
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
