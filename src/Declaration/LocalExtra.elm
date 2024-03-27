module Declaration.LocalExtra exposing (nameAlter, names, references, subReferencesAlter, usedModules)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Expression.LocalExtra
import Pattern.LocalExtra
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


{-| Declared name (+ possible variant names)
-}
names : Declaration -> Set String
names =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .name
                    |> Elm.Syntax.Node.value
                    |> Set.singleton

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                typeAliasDeclaration.name |> Elm.Syntax.Node.value |> Set.singleton

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                variantType.constructors
                    |> List.map (\(Elm.Syntax.Node.Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
                    |> Set.fromList
                    |> Set.insert (variantType.name |> Elm.Syntax.Node.value)

            Elm.Syntax.Declaration.PortDeclaration signature ->
                signature.name |> Elm.Syntax.Node.value |> Set.singleton

            Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
                infixDeclaration.operator |> Elm.Syntax.Node.value |> Set.singleton

            -- invalid
            Elm.Syntax.Declaration.Destructuring _ _ ->
                Set.empty


nameAlter : (String -> String) -> (Declaration -> Declaration)
nameAlter nameChange =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                Elm.Syntax.Declaration.FunctionDeclaration
                    { signature =
                        functionDeclaration.signature
                            |> Maybe.map
                                (Elm.Syntax.Node.map
                                    (\signature ->
                                        { name = signature.name |> Elm.Syntax.Node.map nameChange
                                        , typeAnnotation = signature.typeAnnotation
                                        }
                                    )
                                )
                    , documentation = functionDeclaration.documentation
                    , declaration =
                        functionDeclaration.declaration
                            |> Elm.Syntax.Node.map
                                (\implementation ->
                                    { name = implementation.name |> Elm.Syntax.Node.map nameChange
                                    , arguments = implementation.arguments
                                    , expression = implementation.expression
                                    }
                                )
                    }

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                Elm.Syntax.Declaration.AliasDeclaration
                    { name = typeAliasDeclaration.name |> Elm.Syntax.Node.map nameChange
                    , documentation = typeAliasDeclaration.documentation
                    , generics = typeAliasDeclaration.generics
                    , typeAnnotation = typeAliasDeclaration.typeAnnotation
                    }

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                Elm.Syntax.Declaration.CustomTypeDeclaration
                    { name = variantType.name |> Elm.Syntax.Node.map nameChange
                    , documentation = variantType.documentation
                    , generics = variantType.generics
                    , constructors =
                        variantType.constructors
                            |> List.map
                                (\variantNode ->
                                    variantNode
                                        |> Elm.Syntax.Node.map
                                            (\variant ->
                                                { name = variant.name |> Elm.Syntax.Node.map nameChange
                                                , arguments = variant.arguments
                                                }
                                            )
                                )
                    }

            Elm.Syntax.Declaration.PortDeclaration signature ->
                Elm.Syntax.Declaration.PortDeclaration
                    { name = signature.name |> Elm.Syntax.Node.map nameChange
                    , typeAnnotation = signature.typeAnnotation
                    }

            -- not supported
            Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
                Elm.Syntax.Declaration.InfixDeclaration infixDeclaration

            -- invalid
            Elm.Syntax.Declaration.Destructuring pattern toDestructure ->
                Elm.Syntax.Declaration.Destructuring pattern toDestructure


subReferencesAlter :
    (( Elm.Syntax.ModuleName.ModuleName, String ) -> ( Elm.Syntax.ModuleName.ModuleName, String ))
    -> (Declaration -> Declaration)
subReferencesAlter referenceAlter =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                Elm.Syntax.Declaration.FunctionDeclaration
                    { documentation = functionDeclaration.documentation
                    , signature =
                        functionDeclaration.signature
                            |> Maybe.map
                                (Elm.Syntax.Node.map
                                    (\signature ->
                                        { name = signature.name
                                        , typeAnnotation =
                                            signature.typeAnnotation
                                                |> Elm.Syntax.Node.map
                                                    (Type.LocalExtra.referencesAlter referenceAlter)
                                        }
                                    )
                                )
                    , declaration =
                        functionDeclaration.declaration
                            |> Elm.Syntax.Node.map
                                (\implementation ->
                                    { name = implementation.name
                                    , arguments =
                                        implementation.arguments
                                            |> List.map (Elm.Syntax.Node.map (Pattern.LocalExtra.referencesAlter referenceAlter))
                                    , expression =
                                        implementation.expression
                                            |> Elm.Syntax.Node.map (Expression.LocalExtra.referencesAlter referenceAlter)
                                    }
                                )
                    }

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                Elm.Syntax.Declaration.AliasDeclaration
                    { documentation = typeAliasDeclaration.documentation
                    , name = typeAliasDeclaration.name
                    , generics = typeAliasDeclaration.generics
                    , typeAnnotation =
                        typeAliasDeclaration.typeAnnotation
                            |> Elm.Syntax.Node.map (Type.LocalExtra.referencesAlter referenceAlter)
                    }

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                Elm.Syntax.Declaration.CustomTypeDeclaration
                    { documentation = variantType.documentation
                    , name = variantType.name
                    , generics = variantType.generics
                    , constructors =
                        variantType.constructors
                            |> List.map
                                (\variantNode ->
                                    variantNode
                                        |> Elm.Syntax.Node.map
                                            (\variant ->
                                                { name = variant.name
                                                , arguments =
                                                    variant.arguments
                                                        |> List.map (Elm.Syntax.Node.map (Type.LocalExtra.referencesAlter referenceAlter))
                                                }
                                            )
                                )
                    }

            Elm.Syntax.Declaration.PortDeclaration signature ->
                Elm.Syntax.Declaration.PortDeclaration
                    { name = signature.name
                    , typeAnnotation =
                        signature.typeAnnotation
                            |> Elm.Syntax.Node.map (Type.LocalExtra.referencesAlter referenceAlter)
                    }

            -- not supported
            Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
                Elm.Syntax.Declaration.InfixDeclaration infixDeclaration

            -- invalid
            Elm.Syntax.Declaration.Destructuring pattern toDestructure ->
                Elm.Syntax.Declaration.Destructuring pattern toDestructure


usedModules : Declaration -> Set Elm.Syntax.ModuleName.ModuleName
usedModules =
    \declaration ->
        declaration
            |> references
            |> Set.map (\( moduleName, _ ) -> moduleName)
            |> Set.remove []


references : Declaration -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
references =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                let
                    argumentPatterns : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    argumentPatterns =
                        functionDeclaration.declaration
                            |> Elm.Syntax.Node.value
                            |> .arguments
                in
                [ case functionDeclaration.signature of
                    Nothing ->
                        Set.empty

                    Just (Elm.Syntax.Node.Node _ signature) ->
                        signature
                            |> .typeAnnotation
                            |> Type.LocalExtra.nodeReferences
                , functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .expression
                    |> Expression.LocalExtra.nodeReferencesWithBranchLocalVariables
                        (argumentPatterns |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.nodeVariables)
                , argumentPatterns
                    |> Set.LocalExtra.unionFromListMap (\patternNode -> patternNode |> Pattern.LocalExtra.nodeReferences)
                ]
                    |> Set.LocalExtra.unionFromList

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                typeAliasDeclaration.typeAnnotation |> Type.LocalExtra.nodeReferences

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                variantType.constructors
                    |> List.concatMap (\(Elm.Syntax.Node.Node _ variant) -> variant.arguments)
                    |> Set.LocalExtra.unionFromListMap
                        (\attachmentType -> attachmentType |> Type.LocalExtra.nodeReferences)

            Elm.Syntax.Declaration.PortDeclaration signature ->
                signature.typeAnnotation |> Type.LocalExtra.nodeReferences

            -- not supported
            Elm.Syntax.Declaration.InfixDeclaration _ ->
                Set.empty

            -- invalid
            Elm.Syntax.Declaration.Destructuring _ _ ->
                Set.empty
