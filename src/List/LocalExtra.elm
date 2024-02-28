module List.LocalExtra exposing (allJustMap, firstJustMap)


allJustMap : (a -> Maybe b) -> List a -> Maybe (List b)
allJustMap mapToMaybe =
    \list -> allJustMapOnto [] mapToMaybe list


allJustMapOnto : List b -> (a -> Maybe b) -> List a -> Maybe (List b)
allJustMapOnto soFar mapToMaybe list =
    case list of
        [] ->
            soFar |> List.reverse |> Just

        head :: tail ->
            case mapToMaybe head of
                Just a ->
                    allJustMapOnto (a :: soFar) mapToMaybe tail

                Nothing ->
                    Nothing


firstJustMap : (a -> Maybe b) -> List a -> Maybe b
firstJustMap mapToMaybe list =
    case list of
        [] ->
            Nothing

        node :: rest ->
            case mapToMaybe node of
                Just value ->
                    Just value

                Nothing ->
                    firstJustMap mapToMaybe rest
