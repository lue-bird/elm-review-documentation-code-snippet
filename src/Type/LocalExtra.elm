module Type.LocalExtra exposing (nodeReferences, referencesAlter, usedModules)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation
import List.LocalExtra
import Set exposing (Set)


referencesAlter :
    (( Elm.Syntax.ModuleName.ModuleName, String ) -> ( Elm.Syntax.ModuleName.ModuleName, String ))
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
referencesAlter referenceAlter =
    \type_ ->
        type_
            |> map
                (\innerType ->
                    case innerType of
                        Elm.Syntax.TypeAnnotation.Typed nameNode arguments ->
                            Elm.Syntax.TypeAnnotation.Typed
                                (nameNode
                                    |> Elm.Syntax.Node.map
                                        (\( moduleName, unqualifiedName ) ->
                                            ( moduleName, unqualifiedName ) |> referenceAlter
                                        )
                                )
                                arguments

                        otherType ->
                            otherType
                )


{-| Map it, then all its sub-types, all the way down
-}
map :
    (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> (Elm.Syntax.TypeAnnotation.TypeAnnotation -> Elm.Syntax.TypeAnnotation.TypeAnnotation)
map typeChange =
    let
        step : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        step =
            Elm.Syntax.Node.map (\stepType -> stepType |> map typeChange)
    in
    -- IGNORE TCO
    \type_ ->
        case type_ |> typeChange of
            Elm.Syntax.TypeAnnotation.Unit ->
                Elm.Syntax.TypeAnnotation.Unit

            Elm.Syntax.TypeAnnotation.GenericType name ->
                Elm.Syntax.TypeAnnotation.GenericType name

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (input |> step) (output |> step)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                Elm.Syntax.TypeAnnotation.Tupled (parts |> List.map step)

            Elm.Syntax.TypeAnnotation.Record fields ->
                Elm.Syntax.TypeAnnotation.Record
                    (fields |> List.map (Elm.Syntax.Node.map (\( name, value ) -> ( name, value |> step ))))

            Elm.Syntax.TypeAnnotation.GenericRecord extended fields ->
                Elm.Syntax.TypeAnnotation.GenericRecord extended
                    (fields
                        |> Elm.Syntax.Node.map
                            (List.map (Elm.Syntax.Node.map (\( name, value ) -> ( name, value |> step ))))
                    )

            Elm.Syntax.TypeAnnotation.Typed nameNode arguments ->
                Elm.Syntax.TypeAnnotation.Typed nameNode (arguments |> List.map step)


references : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
references =
    \type_ ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType _ ->
                Set.empty

            Elm.Syntax.TypeAnnotation.Unit ->
                Set.empty

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                Set.union (input |> nodeReferences) (output |> nodeReferences)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                parts |> List.LocalExtra.setUnionMap nodeReferences

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields |> List.LocalExtra.setUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> nodeReferences)

            Elm.Syntax.TypeAnnotation.GenericRecord _ (Node _ fields) ->
                fields |> List.LocalExtra.setUnionMap (\(Node _ ( _, fieldValue )) -> fieldValue |> nodeReferences)

            Elm.Syntax.TypeAnnotation.Typed (Node _ ( moduleName, unqualifiedName )) arguments ->
                arguments
                    |> List.LocalExtra.setUnionMap nodeReferences
                    |> Set.insert ( moduleName, unqualifiedName )


usedModules : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set Elm.Syntax.ModuleName.ModuleName
usedModules =
    \type_ ->
        type_
            |> references
            |> Set.map (\( moduleName, _ ) -> moduleName)
            |> Set.remove []


nodeReferences : Node Elm.Syntax.TypeAnnotation.TypeAnnotation -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
nodeReferences =
    \(Node _ type_) -> type_ |> references
