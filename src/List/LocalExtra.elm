module List.LocalExtra exposing (allJustMap, consJust, firstJustMap, removeLast)


consJust : Maybe a -> (List a -> List a)
consJust newHeadMaybe =
    \list ->
        case newHeadMaybe of
            Nothing ->
                list

            Just newHead ->
                newHead :: list


removeLast : List a -> List a
removeLast =
    \list ->
        case list of
            [] ->
                []

            [ _ ] ->
                []

            el0 :: el1 :: el2Up ->
                el0 :: removeLast (el1 :: el2Up)


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
