module Expression.LocalExtra exposing (fullyQualify, references, usedModules)

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Imports exposing (Imports)
import List.LocalExtra
import Origin
import Pattern.LocalExtra
import Set exposing (Set)
import Type.LocalExtra


fullyQualify : Imports -> (Expression -> Expression)
fullyQualify imports =
    \expression ->
        expression
            |> map
                (\innerExpression ->
                    case innerExpression of
                        Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                            Elm.Syntax.Expression.FunctionOrValue
                                (( qualification, unqualifiedName ) |> Origin.determine imports)
                                unqualifiedName

                        Elm.Syntax.Expression.LambdaExpression lambda ->
                            Elm.Syntax.Expression.LambdaExpression
                                { args = lambda.args |> List.map (Elm.Syntax.Node.map (Pattern.LocalExtra.fullyQualify imports))
                                , expression = lambda.expression
                                }

                        Elm.Syntax.Expression.CaseExpression caseOf ->
                            Elm.Syntax.Expression.CaseExpression
                                { expression = caseOf.expression
                                , cases =
                                    caseOf.cases
                                        |> List.map
                                            (\( patternNode, expressionNode ) ->
                                                ( patternNode |> Elm.Syntax.Node.map (Pattern.LocalExtra.fullyQualify imports)
                                                , expressionNode
                                                )
                                            )
                                }

                        Elm.Syntax.Expression.LetExpression letIn ->
                            Elm.Syntax.Expression.LetExpression
                                { expression = letIn.expression
                                , declarations =
                                    letIn.declarations
                                        |> List.map
                                            (Elm.Syntax.Node.map
                                                (\letDeclaration ->
                                                    case letDeclaration of
                                                        Elm.Syntax.Expression.LetDestructuring patternNode expressionNode ->
                                                            Elm.Syntax.Expression.LetDestructuring
                                                                (patternNode |> Elm.Syntax.Node.map (Pattern.LocalExtra.fullyQualify imports))
                                                                expressionNode

                                                        Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                                            Elm.Syntax.Expression.LetFunction
                                                                { documentation = letValueOrFunctionDeclaration.documentation
                                                                , signature =
                                                                    case letValueOrFunctionDeclaration.signature of
                                                                        Nothing ->
                                                                            Nothing

                                                                        Just signatureNode ->
                                                                            signatureNode
                                                                                |> Elm.Syntax.Node.map
                                                                                    (\signature ->
                                                                                        { name = signature.name
                                                                                        , typeAnnotation =
                                                                                            signature.typeAnnotation
                                                                                                |> Elm.Syntax.Node.map (Type.LocalExtra.fullyQualify imports)
                                                                                        }
                                                                                    )
                                                                                |> Just
                                                                , declaration =
                                                                    letValueOrFunctionDeclaration.declaration
                                                                        |> Elm.Syntax.Node.map
                                                                            (\declaration ->
                                                                                { expression = declaration.expression
                                                                                , name = declaration.name
                                                                                , arguments =
                                                                                    declaration.arguments
                                                                                        |> List.map (Elm.Syntax.Node.map (Pattern.LocalExtra.fullyQualify imports))
                                                                                }
                                                                            )
                                                                }
                                                )
                                            )
                                }

                        otherExpression ->
                            otherExpression
                )


{-| Map it, then all its sub-expressions, all the way down
-}
map : (Expression -> Expression) -> (Expression -> Expression)
map expressionChange =
    -- IGNORE TCO
    \expression ->
        let
            step : Node Expression -> Node Expression
            step =
                Elm.Syntax.Node.map (\stepExpression -> stepExpression |> map expressionChange)
        in
        case expression |> expressionChange of
            Elm.Syntax.Expression.LetExpression letBlock ->
                Elm.Syntax.Expression.LetExpression
                    { expression = letBlock.expression |> step
                    , declarations =
                        letBlock.declarations
                            |> List.map
                                (Elm.Syntax.Node.map
                                    (\letDeclaration ->
                                        case letDeclaration of
                                            Elm.Syntax.Expression.LetFunction letFunction ->
                                                Elm.Syntax.Expression.LetFunction
                                                    { letFunction
                                                        | declaration =
                                                            letFunction.declaration
                                                                |> Elm.Syntax.Node.map (\fun -> { fun | expression = fun.expression |> step })
                                                    }

                                            Elm.Syntax.Expression.LetDestructuring pattern expression_ ->
                                                Elm.Syntax.Expression.LetDestructuring pattern (expression_ |> step)
                                    )
                                )
                    }

            Elm.Syntax.Expression.ListExpr expressions ->
                Elm.Syntax.Expression.ListExpr (expressions |> List.map step)

            Elm.Syntax.Expression.TupledExpression expressions ->
                Elm.Syntax.Expression.TupledExpression (expressions |> List.map step)

            Elm.Syntax.Expression.RecordExpr fields ->
                Elm.Syntax.Expression.RecordExpr (fields |> List.map (Elm.Syntax.Node.map (Tuple.mapSecond step)))

            Elm.Syntax.Expression.RecordUpdateExpression recordVariable setters ->
                Elm.Syntax.Expression.RecordUpdateExpression recordVariable
                    (setters |> List.map (Elm.Syntax.Node.map (Tuple.mapSecond step)))

            Elm.Syntax.Expression.RecordAccess recordToAccess fieldName ->
                Elm.Syntax.Expression.RecordAccess (recordToAccess |> step) fieldName

            Elm.Syntax.Expression.Application applicationElements ->
                Elm.Syntax.Expression.Application (applicationElements |> List.map step)

            Elm.Syntax.Expression.CaseExpression caseBlock ->
                Elm.Syntax.Expression.CaseExpression
                    { expression = caseBlock.expression
                    , cases = caseBlock.cases |> List.map (Tuple.mapSecond step)
                    }

            Elm.Syntax.Expression.OperatorApplication symbol direction left right ->
                Elm.Syntax.Expression.OperatorApplication symbol direction (left |> step) (right |> step)

            Elm.Syntax.Expression.IfBlock condition then_ else_ ->
                Elm.Syntax.Expression.IfBlock (condition |> step) (then_ |> step) (else_ |> step)

            Elm.Syntax.Expression.LambdaExpression lambda ->
                Elm.Syntax.Expression.LambdaExpression { lambda | expression = lambda.expression |> step }

            Elm.Syntax.Expression.ParenthesizedExpression expressionInParens ->
                Elm.Syntax.Expression.ParenthesizedExpression (expressionInParens |> step)

            Elm.Syntax.Expression.Negation expressionInNegation ->
                Elm.Syntax.Expression.Negation (expressionInNegation |> step)

            Elm.Syntax.Expression.UnitExpr ->
                Elm.Syntax.Expression.UnitExpr

            Elm.Syntax.Expression.Integer int ->
                Elm.Syntax.Expression.Integer int

            Elm.Syntax.Expression.Hex int ->
                Elm.Syntax.Expression.Hex int

            Elm.Syntax.Expression.Floatable float ->
                Elm.Syntax.Expression.Floatable float

            Elm.Syntax.Expression.Literal string ->
                Elm.Syntax.Expression.Literal string

            Elm.Syntax.Expression.CharLiteral char ->
                Elm.Syntax.Expression.CharLiteral char

            Elm.Syntax.Expression.GLSLExpression glsl ->
                Elm.Syntax.Expression.GLSLExpression glsl

            Elm.Syntax.Expression.RecordAccessFunction fieldName ->
                Elm.Syntax.Expression.RecordAccessFunction fieldName

            Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName

            Elm.Syntax.Expression.Operator symbol ->
                Elm.Syntax.Expression.Operator symbol

            Elm.Syntax.Expression.PrefixOperator symbol ->
                Elm.Syntax.Expression.PrefixOperator symbol


usedModules : Expression -> Set Elm.Syntax.ModuleName.ModuleName
usedModules =
    \expression ->
        expression
            |> references
            |> Set.map (\( moduleName, _ ) -> moduleName)
            |> Set.remove []


references : Expression -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
references =
    -- IGNORE TCO
    \expression ->
        Set.union
            (expression
                |> subs
                |> List.LocalExtra.setUnionMap
                    (\(Node _ innerExpression) ->
                        innerExpression |> references
                    )
            )
            (case expression of
                Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                    ( qualification, unqualifiedName ) |> Set.singleton

                Elm.Syntax.Expression.LambdaExpression lambda ->
                    lambda.args |> List.LocalExtra.setUnionMap Pattern.LocalExtra.nodeReferences

                Elm.Syntax.Expression.CaseExpression caseOf ->
                    caseOf.cases
                        |> List.LocalExtra.setUnionMap
                            (\( Node _ pattern, _ ) -> pattern |> Pattern.LocalExtra.references)

                Elm.Syntax.Expression.LetExpression letIn ->
                    letIn.declarations
                        |> List.LocalExtra.setUnionMap
                            (\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetDestructuring (Node _ pattern) _ ->
                                        pattern |> Pattern.LocalExtra.references

                                    Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                        Set.union
                                            (case letValueOrFunctionDeclaration.signature of
                                                Nothing ->
                                                    Set.empty

                                                Just (Node _ signature) ->
                                                    signature.typeAnnotation
                                                        |> Type.LocalExtra.nodeReferences
                                            )
                                            (letValueOrFunctionDeclaration.declaration
                                                |> Elm.Syntax.Node.value
                                                |> .arguments
                                                |> List.LocalExtra.setUnionMap (\(Node _ pattern) -> pattern |> Pattern.LocalExtra.references)
                                            )
                            )

                _ ->
                    Set.empty
            )


{-| Get all immediate child expressions of an expression
-}
subs : Expression -> List (Node Expression)
subs expression =
    case expression of
        Elm.Syntax.Expression.LetExpression letBlock ->
            letBlock.expression
                :: (letBlock.declarations
                        |> List.map
                            (\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction letFunction ->
                                        letFunction.declaration |> Elm.Syntax.Node.value |> .expression

                                    Elm.Syntax.Expression.LetDestructuring _ expression_ ->
                                        expression_
                            )
                   )

        Elm.Syntax.Expression.ListExpr expressions ->
            expressions

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.RecordExpr fields ->
            fields |> List.map (\(Node _ ( _, value )) -> value)

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            setters |> List.map (\(Node _ ( _, newValue )) -> newValue)

        Elm.Syntax.Expression.RecordAccess recordToAccess _ ->
            [ recordToAccess ]

        Elm.Syntax.Expression.Application applicationElements ->
            applicationElements

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases |> List.map (\( _, caseExpression ) -> caseExpression))

        Elm.Syntax.Expression.OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        Elm.Syntax.Expression.IfBlock condition then_ else_ ->
            [ condition, then_, else_ ]

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.ParenthesizedExpression expressionInParens ->
            [ expressionInParens ]

        Elm.Syntax.Expression.Negation expressionInNegation ->
            [ expressionInNegation ]

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.PrefixOperator _ ->
            []
