module Declaration.LocalExtra exposing (fullyQualify, usedModules)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Expression.LocalExtra
import Imports exposing (Imports)
import List.LocalExtra
import Pattern.LocalExtra
import Set exposing (Set)
import Type.LocalExtra


fullyQualify : Imports -> (Declaration -> Declaration)
fullyQualify imports =
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
                                                    (Type.LocalExtra.fullyQualify imports)
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
                                            |> List.map (Elm.Syntax.Node.map (Pattern.LocalExtra.fullyQualify imports))
                                    , expression =
                                        implementation.expression
                                            |> Elm.Syntax.Node.map (Expression.LocalExtra.fullyQualify imports)
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
                            |> Elm.Syntax.Node.map (Type.LocalExtra.fullyQualify imports)
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
                                                { variant
                                                    | arguments =
                                                        variant.arguments
                                                            |> List.map (Elm.Syntax.Node.map (Type.LocalExtra.fullyQualify imports))
                                                }
                                            )
                                )
                    }

            Elm.Syntax.Declaration.PortDeclaration signature ->
                Elm.Syntax.Declaration.PortDeclaration
                    { signature
                        | typeAnnotation =
                            signature.typeAnnotation
                                |> Elm.Syntax.Node.map (Type.LocalExtra.fullyQualify imports)
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
    -- IGNORE TCO
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                [ case functionDeclaration.signature of
                    Nothing ->
                        Set.empty

                    Just (Node _ signature) ->
                        signature
                            |> .typeAnnotation
                            |> Type.LocalExtra.nodeReferences
                , functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .expression
                    |> Elm.Syntax.Node.value
                    |> Expression.LocalExtra.references
                , functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .arguments
                    |> List.LocalExtra.setUnionMap (\patternNode -> patternNode |> Pattern.LocalExtra.nodeReferences)
                ]
                    |> List.LocalExtra.setUnionMap identity

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                typeAliasDeclaration.typeAnnotation |> Type.LocalExtra.nodeReferences

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                variantType.constructors
                    |> List.concatMap (\(Node _ variant) -> variant.arguments)
                    |> List.LocalExtra.setUnionMap
                        (\attachmentType -> attachmentType |> Type.LocalExtra.nodeReferences)

            Elm.Syntax.Declaration.PortDeclaration signature ->
                Debug.todo ""

            -- not supported
            Elm.Syntax.Declaration.InfixDeclaration _ ->
                Set.empty

            -- invalid
            Elm.Syntax.Declaration.Destructuring _ _ ->
                Set.empty
