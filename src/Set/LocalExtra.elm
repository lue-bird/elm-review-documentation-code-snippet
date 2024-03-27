module Set.LocalExtra exposing (fromListMap, justsMap, unionFromList, unionFromListMap)

import Set exposing (Set)


unionFromList : List (Set comparable) -> Set comparable
unionFromList =
    \list ->
        list |> unionFromListMap identity


unionFromListMap : (a -> Set comparable) -> (List a -> Set comparable)
unionFromListMap elementToSet =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    Set.union soFar (element |> elementToSet)
                )
                Set.empty


fromListMap : (element -> comparableElement) -> (List element -> Set comparableElement)
fromListMap toComparable =
    \list ->
        list
            |> List.foldl
                (\element acc ->
                    acc |> Set.insert (element |> toComparable)
                )
                Set.empty


justsMap : (element -> Maybe comparableValue) -> (Set element -> Set comparableValue)
justsMap toMaybe =
    \set ->
        set
            |> Set.foldl
                (\element soFar ->
                    case element |> toMaybe of
                        Nothing ->
                            soFar

                        Just value ->
                            soFar |> Set.insert value
                )
                Set.empty
