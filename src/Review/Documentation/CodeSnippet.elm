module Review.Documentation.CodeSnippet exposing (check)

{-| Reports choice `type` parameters that aren't used in the definition (often called "opaque types").

    import Review.Documentation.CodeSnippet

    config =
        [ Review.Documentation.CodeSnippet.check
        ]

@docs check

-}

import Declaration.LocalExtra
import Dict
import Elm.CodeGen
import Elm.Docs
import Elm.Pretty
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation
import ElmSyntaxParse
import Expression.LocalExtra
import FastDict exposing (Dict)
import Imports
import List.LocalExtra
import Origin
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Review.Fix
import Review.Project.Dependency
import Review.Rule exposing (ModuleKey, Rule)
import RoughMarkdown
import Set exposing (Set)
import Type.LocalExtra


type alias ProjectContext =
    { documentationCodeSnippetTestModuleKey : Maybe ModuleKey
    , byModule :
        Dict
            Elm.Syntax.ModuleName.ModuleName
            { exposedChoiceTypesExposingVariants : Dict String (Set String)
            , exposedValueAndFunctionAndTypeAliasNames : Set String
            , foundDocumentationExamplesInModuleHeader : List CodeSnippet
            , foundDocumentationExamplesInMembers : Dict String (List CodeSnippet)
            }
    , foundDocumentationExamplesInReadme : List CodeSnippet
    }


type alias CodeSnippet =
    { imports : List Elm.Syntax.Import.Import
    , declarations : List Elm.Syntax.Declaration.Declaration
    , checks : List CodeSnippetCheck
    }


type alias CodeSnippetCheck =
    { expectation : CodeSnippetExpectation
    , checkedExpression : Elm.Syntax.Expression.Expression
    }


type CodeSnippetExpectation
    = Equals Elm.Syntax.Expression.Expression
    | IsOfType Elm.Syntax.TypeAnnotation.TypeAnnotation


type ModuleContext
    = NonTestModuleContext NonTestModuleContext
    | TestModuleContext ModuleKey


type alias NonTestModuleContext =
    RecordWithoutConstructorFunction
        { exposingKind : ExposingKind
        , exposedValueAndFunctionAndTypeAliasNames : Set String
        , exposedChoiceTypesExposingVariants : Dict String (Set String)
        , foundDocumentationExamplesInModuleHeader : List CodeSnippet
        , foundDocumentationExamplesInMembers : Dict String (List CodeSnippet)
        }


type ExposingKind
    = ExposingExplicit
    | ExposingAll


moduleHeaderExposed :
    Elm.Syntax.Module.Module
    ->
        { kind : ExposingKind
        , exposedValueAndFunctionAndTypeAliasNames : Set String
        , exposedChoiceTypesExposingVariants : Set String
        }
moduleHeaderExposed =
    \moduleHeader ->
        case moduleHeader of
            Elm.Syntax.Module.NormalModule defaultModuleHeaderData ->
                defaultModuleHeaderData.exposingList |> Elm.Syntax.Node.value |> syntaxExposingToExposed

            Elm.Syntax.Module.PortModule defaultModuleHeaderData ->
                defaultModuleHeaderData.exposingList |> Elm.Syntax.Node.value |> syntaxExposingToExposed

            Elm.Syntax.Module.EffectModule effectModuleHeaderData ->
                effectModuleHeaderData.exposingList |> Elm.Syntax.Node.value |> syntaxExposingToExposed


syntaxExposingToExposed :
    Elm.Syntax.Exposing.Exposing
    ->
        { kind : ExposingKind
        , exposedValueAndFunctionAndTypeAliasNames : Set String
        , exposedChoiceTypesExposingVariants : Set String
        }
syntaxExposingToExposed =
    \exposing_ ->
        case exposing_ of
            Elm.Syntax.Exposing.Explicit list ->
                { kind = ExposingExplicit
                , exposedValueAndFunctionAndTypeAliasNames =
                    list
                        |> List.filterMap
                            (\(Node _ expose) ->
                                case expose of
                                    Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                        name |> Just

                                    Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                        case typeExpose.open of
                                            Just _ ->
                                                Nothing

                                            Nothing ->
                                                typeExpose.name |> Just

                                    Elm.Syntax.Exposing.FunctionExpose functionExpose ->
                                        functionExpose |> Just

                                    Elm.Syntax.Exposing.InfixExpose _ ->
                                        Nothing
                            )
                        |> Set.fromList
                , exposedChoiceTypesExposingVariants =
                    list
                        |> List.filterMap
                            (\(Node _ expose) ->
                                case expose of
                                    Elm.Syntax.Exposing.TypeOrAliasExpose _ ->
                                        Nothing

                                    Elm.Syntax.Exposing.FunctionExpose _ ->
                                        Nothing

                                    Elm.Syntax.Exposing.InfixExpose _ ->
                                        Nothing

                                    Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                        case typeExpose.open of
                                            Nothing ->
                                                Nothing

                                            Just _ ->
                                                typeExpose.name |> Just
                            )
                        |> Set.fromList
                }

            Elm.Syntax.Exposing.All _ ->
                { kind = ExposingAll
                , exposedValueAndFunctionAndTypeAliasNames = Set.empty
                , exposedChoiceTypesExposingVariants = Set.empty
                }


{-| [`Rule`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#Rule) to generate tests from your documentation examples.

There are two kinds of checks:


### value checks

    module Dict.Extra exposing (keySet)

    {-| `Dict.keys` but returning a `Set` instead of a `List`.

        import Dict
        import Set

        type Letter
            = A
            | B
            | C

        Dict.fromList [ ( 0, A ), ( 1, B ), ( 2, C ) ]
            |> keySet
        --> Set.fromList [ 0, 1, 2 ]

        -- or
        Dict.fromList [ ( 0, A ), ( 1, B ), ( 2, C ) ]
            |> keySet
        -->
        Set.fromList
            [ 0
            , 1
            , 2
            ]

        -- or
        Dict.fromList [ ( 0, A ), ( 1, B ), ( 2, C ) ]
            |> keySet
        --> Set.fromList
        -->     [ 0
        -->     , 1
        -->     , 2
        -->     ]

    -}
    keySet = ...

This verifies that the actual value of the expression before `-->` is the same as the expected value after `-->`.


### type checks

    module Codec exposing (Codec, record, field, recordFinish, signedInt)

    {-| Start creating a codec for a record.

        type alias Point =
            { x : Int, y : Int }

        Codec.record (\x y -> { x = x, y = y })
            |> Codec.field .x Codec.signedInt
            |> Codec.field .y Codec.signedInt
            |> Codec.recordFinish
        --: Codec Point
    -}
    record = ...

Both checks will be run on _all modules_ and the readme. If you want to disable this for e.g. generated or vendored code,
use [`Review.Rule.ignoreErrorsForDirectories`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#ignoreErrorsForDirectories)

-}
check : Rule
check =
    Review.Rule.newProjectRuleSchema "Review.Documentation.Example.check" initialProjectContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withReadmeProjectVisitor
            (\maybeReadme context ->
                ( []
                , case maybeReadme of
                    Nothing ->
                        context

                    Just readme ->
                        { context
                            | foundDocumentationExamplesInReadme =
                                readme.content
                                    |> markdownElmCodeBlocksInReadme
                                    |> List.filterMap elmCodeBlockToSnippet
                        }
                )
            )
        |> Review.Rule.withDirectDependenciesProjectVisitor
            (\dependencyDocsDict context ->
                ( []
                , { context
                    | byModule =
                        FastDict.union context.byModule
                            (dependencyDocsDict
                                |> Dict.values
                                |> List.concatMap (\dependencyDocs -> dependencyDocs |> Review.Project.Dependency.modules)
                                |> List.map
                                    (\moduleDocs ->
                                        ( moduleDocs.name |> String.split "."
                                        , { exposedChoiceTypesExposingVariants =
                                                moduleDocs
                                                    |> .unions
                                                    |> List.filterMap
                                                        (\choiceType ->
                                                            case choiceType.tags of
                                                                [] ->
                                                                    Nothing

                                                                variant0 :: variant1Up ->
                                                                    ( choiceType.name
                                                                    , (variant0 :: variant1Up)
                                                                        |> List.map (\( variantName, _ ) -> variantName)
                                                                        |> Set.fromList
                                                                    )
                                                                        |> Just
                                                        )
                                                    |> FastDict.fromList
                                          , exposedValueAndFunctionAndTypeAliasNames =
                                                moduleDocs |> moduleDocsValueAndFunctionAndTypeAliasNames
                                          , foundDocumentationExamplesInModuleHeader = []
                                          , foundDocumentationExamplesInMembers = FastDict.empty
                                          }
                                        )
                                    )
                                |> FastDict.fromList
                            )
                  }
                )
            )
        |> Review.Rule.withModuleVisitor
            (\moduleRuleSchema ->
                moduleRuleSchema
                    |> Review.Rule.withModuleDefinitionVisitor
                        (\(Node _ moduleHeader) context ->
                            ( []
                            , case context of
                                TestModuleContext testModuleContext ->
                                    testModuleContext |> TestModuleContext

                                NonTestModuleContext nonTestModuleContext ->
                                    let
                                        exposingInfo : { kind : ExposingKind, exposedValueAndFunctionAndTypeAliasNames : Set String, exposedChoiceTypesExposingVariants : Set String }
                                        exposingInfo =
                                            moduleHeader |> moduleHeaderExposed
                                    in
                                    { nonTestModuleContext
                                        | exposingKind = exposingInfo.kind
                                        , exposedValueAndFunctionAndTypeAliasNames = exposingInfo.exposedValueAndFunctionAndTypeAliasNames
                                        , exposedChoiceTypesExposingVariants =
                                            exposingInfo.exposedChoiceTypesExposingVariants
                                                |> Set.toList
                                                |> List.map (\choiceTypeName -> ( choiceTypeName, Set.empty ))
                                                |> FastDict.fromList
                                    }
                                        |> NonTestModuleContext
                            )
                        )
                    |> Review.Rule.withDeclarationListVisitor
                        (\declarations context ->
                            ( []
                            , case context of
                                TestModuleContext testModuleContext ->
                                    testModuleContext |> TestModuleContext

                                NonTestModuleContext nonTestModuleContext ->
                                    nonTestModuleContext |> declarationListVisitor declarations |> NonTestModuleContext
                            )
                        )
                    |> Review.Rule.withDeclarationEnterVisitor
                        (\(Node _ declaration) context ->
                            ( []
                            , case context of
                                TestModuleContext testModuleContext ->
                                    testModuleContext |> TestModuleContext

                                NonTestModuleContext nonTestModuleContext ->
                                    case declaration |> declarationToDocumented of
                                        Nothing ->
                                            nonTestModuleContext |> NonTestModuleContext

                                        Just memberDocumented ->
                                            { nonTestModuleContext
                                                | foundDocumentationExamplesInMembers =
                                                    nonTestModuleContext.foundDocumentationExamplesInMembers
                                                        |> FastDict.insert memberDocumented.name
                                                            (memberDocumented.documentation
                                                                |> markdownElmCodeBlocksInModule
                                                                |> List.filterMap elmCodeBlockToSnippet
                                                            )
                                            }
                                                |> NonTestModuleContext
                            )
                        )
            )
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = projectToModuleContextCreator
            , fromModuleToProject = moduleToProjectContextCreator
            , foldProjectContexts = projectContextsMerge
            }
        |> Review.Rule.withFinalProjectEvaluation checkFullProject
        |> Review.Rule.fromProjectRuleSchema


checkFullProject : ProjectContext -> List (Review.Rule.Error { useErrorForModule : () })
checkFullProject =
    \context ->
        case context.documentationCodeSnippetTestModuleKey of
            Nothing ->
                [ Review.Rule.globalError
                    { message = "documentation code snippet test module needs to be added"
                    , details =
                        [ "We need a module to generate documentation code snippet tests in. Please add a module tests/DocumentationCodeSnippet/Test.elm."
                        ]
                    }
                ]

            Just documentationCodeSnippetTestModuleKey ->
                [ Review.Rule.errorForModuleWithFix documentationCodeSnippetTestModuleKey
                    { message = "documentation code snippet test can be added"
                    , details =
                        [ "Adding them will help verify that code blocks in your readme and module documentation work correctly."
                        ]
                    }
                    { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                    [ Review.Fix.replaceRangeBy
                        -- everything
                        { start = { row = 1, column = 1 }, end = { row = 1000000, column = 1 } }
                        ({ foundDocumentationExamplesInReadme = context.foundDocumentationExamplesInReadme
                         , byModule = context.byModule
                         }
                            |> createDocumentationCodeSnippetsTestFile
                            |> Elm.Pretty.pretty 80
                        )
                    ]
                ]


createDocumentationCodeSnippetsTestFile :
    { foundDocumentationExamplesInReadme : List CodeSnippet
    , byModule :
        Dict
            Elm.Syntax.ModuleName.ModuleName
            { moduleInfo_
                | foundDocumentationExamplesInModuleHeader : List CodeSnippet
                , foundDocumentationExamplesInMembers : Dict String (List CodeSnippet)
            }
    }
    -> Elm.CodeGen.File
createDocumentationCodeSnippetsTestFile =
    \info ->
        {- TODO
           - #location is (readme | (moduleName\_\_(header | reference name))) \_\_(code snippet index 0 based)
           - prefix declaration names based on #location
           - map all sub references (type, pattern, value/function) of declarations and actual and expected
               - to fully qualified using Origin.determine
               - if no full qualification is found, see if it's defined in `declarations` and adapt the #location name accordingly
           - bonus: compare `.imports` with full qualifications present in all references and automatically add missing imports as a fix
        -}
        let
            declarations : List Elm.Syntax.Declaration.Declaration
            declarations =
                (info.foundDocumentationExamplesInReadme
                    |> List.concatMap .declarations
                )
                    ++ (info.byModule
                            |> FastDict.values
                            |> List.concatMap
                                (\moduleContext ->
                                    (moduleContext
                                        |> .foundDocumentationExamplesInModuleHeader
                                        |> List.concatMap .declarations
                                    )
                                        ++ (moduleContext
                                                |> .foundDocumentationExamplesInMembers
                                                |> FastDict.values
                                                |> List.concat
                                                |> List.concatMap .declarations
                                           )
                                )
                       )

            codeSnippetTests : List Elm.CodeGen.Expression
            codeSnippetTests =
                (info.foundDocumentationExamplesInReadme |> codeSnippetsToTestWithName "readme")
                    :: (info.byModule
                            |> FastDict.toList
                            |> List.map
                                (\( moduleName, moduleContext ) ->
                                    elmCodeGenTestDescribe (moduleName |> String.join ".")
                                        ((moduleContext
                                            |> .foundDocumentationExamplesInModuleHeader
                                            |> codeSnippetsToTestWithName "module header"
                                         )
                                            :: (moduleContext
                                                    |> .foundDocumentationExamplesInMembers
                                                    |> FastDict.toList
                                                    |> List.map
                                                        (\( documentedDeclaredName, memberCodeSnippets ) ->
                                                            memberCodeSnippets |> codeSnippetsToTestWithName documentedDeclaredName
                                                        )
                                               )
                                            |> List.filterMap identity
                                        )
                                )
                       )
                    |> List.filterMap identity

            codeSnippets : List { imports : List Elm.Syntax.Import.Import, checks : List CodeSnippetCheck, declarations : List Declaration }
            codeSnippets =
                info.foundDocumentationExamplesInReadme
                    ++ (info.byModule
                            |> FastDict.values
                            |> List.concatMap
                                (\moduleContext ->
                                    moduleContext.foundDocumentationExamplesInModuleHeader
                                        ++ (moduleContext
                                                |> .foundDocumentationExamplesInMembers
                                                |> FastDict.values
                                                |> List.concat
                                           )
                                )
                       )

            codeSnippetUsedModules : CodeSnippet -> Set Elm.Syntax.ModuleName.ModuleName
            codeSnippetUsedModules =
                \codeSnippet ->
                    Set.union
                        (codeSnippet.declarations |> List.LocalExtra.setUnionMap Declaration.LocalExtra.usedModules)
                        (codeSnippet.checks |> List.LocalExtra.setUnionMap codeSnippetCheckUsedModules)

            codeSnippetCheckUsedModules : CodeSnippetCheck -> Set Elm.Syntax.ModuleName.ModuleName
            codeSnippetCheckUsedModules =
                \codeSnippetCheck ->
                    Set.union
                        (codeSnippetCheck.checkedExpression |> Expression.LocalExtra.usedModules)
                        (case codeSnippetCheck.expectation of
                            Equals expectedExpression ->
                                expectedExpression |> Expression.LocalExtra.usedModules

                            IsOfType expectedType ->
                                expectedType |> Type.LocalExtra.usedModules
                        )
        in
        Elm.CodeGen.file
            (Elm.CodeGen.normalModule [ "DocumentationCodeSnippet.Test" ] [ Elm.CodeGen.funExpose "tests" ])
            (codeSnippets
                |> List.LocalExtra.setUnionMap codeSnippetUsedModules
                |> Set.insert [ "Expect" ]
                |> Set.insert [ "Test" ]
                |> Set.toList
                |> List.map (\moduleName -> Elm.CodeGen.importStmt moduleName Nothing Nothing)
            )
            (Elm.CodeGen.valDecl
                Nothing
                (Elm.CodeGen.fqTyped [ "Test" ] "Test" [] |> Just)
                "tests"
                (Elm.CodeGen.fqConstruct [ "Test" ]
                    "describe"
                    [ Elm.CodeGen.string "documentation code snippets"
                    , Elm.CodeGen.list
                        (case codeSnippetTests of
                            [] ->
                                [ Elm.CodeGen.fqConstruct [ "Test" ]
                                    "test"
                                    [ Elm.CodeGen.string "currently none. Since having no code snippets is perfectly fine, adding this simple test tells elm-test that everything's good (empty tests fail or throw warnings)"
                                    , Elm.CodeGen.lambda [ Elm.CodeGen.unitPattern ] (Elm.CodeGen.fqConstruct [ "Expect" ] "pass" [])
                                    ]
                                ]

                            codeSnippetTest0 :: codeSnippetTest1Up ->
                                codeSnippetTest0 :: codeSnippetTest1Up
                        )
                    ]
                )
                :: (declarations
                        |> List.map Elm.CodeGen.DeclNoComment
                   )
            )
            (Elm.CodeGen.emptyFileComment
                |> Elm.CodeGen.markdown "automatically generated by [elm-review-documentation-code-snippet](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-documentation-code-snippet/latest)"
                |> Just
            )


elmCodeGenTestDescribe : String -> List Elm.CodeGen.Expression -> Maybe Elm.CodeGen.Expression
elmCodeGenTestDescribe description subTests =
    case subTests of
        [] ->
            Nothing

        subTest0 :: subTest1Up ->
            Elm.CodeGen.fqConstruct [ "Test" ]
                "describe"
                [ Elm.CodeGen.string description
                , Elm.CodeGen.list (subTest0 :: subTest1Up)
                ]
                |> Just


codeSnippetsToTestWithName : String -> List CodeSnippet -> Maybe Elm.CodeGen.Expression
codeSnippetsToTestWithName name =
    \codeSnippets ->
        elmCodeGenTestDescribe name
            (codeSnippets
                |> List.indexedMap
                    (\codeSnippetIndex codeSnippet ->
                        Elm.CodeGen.fqConstruct [ "Test" ]
                            "describe"
                            [ Elm.CodeGen.string ("code snippet " ++ (codeSnippetIndex |> String.fromInt))
                            , codeSnippet.checks
                                |> List.indexedMap
                                    (\checkIndexInSnippet codeSnippetExpectationCheck ->
                                        Elm.CodeGen.fqConstruct [ "Test" ]
                                            "test"
                                            [ Elm.CodeGen.string (checkIndexInSnippet |> String.fromInt)
                                            , Elm.CodeGen.lambda
                                                [ Elm.CodeGen.unitPattern ]
                                                (codeSnippetExpectationCheck |> codeSnippetExpectationCheckToExpectationCode)
                                            ]
                                    )
                                |> Elm.CodeGen.list
                            ]
                    )
            )


codeSnippetExpectationCheckToExpectationCode : CodeSnippetCheck -> Elm.CodeGen.Expression
codeSnippetExpectationCheckToExpectationCode =
    \codeSnippetCheck ->
        case codeSnippetCheck.expectation of
            Equals expectedExpression ->
                Elm.CodeGen.pipe (codeSnippetCheck.checkedExpression |> Elm.CodeGen.parens)
                    [ Elm.CodeGen.fqConstruct [ "Expect" ] "equal" [ expectedExpression ] ]

            IsOfType expectedType ->
                Elm.CodeGen.letExpr
                    [ Elm.Syntax.Expression.LetFunction
                        { documentation = Nothing
                        , signature =
                            { name = "unused" |> Elm.Syntax.Node.empty
                            , typeAnnotation = expectedType |> Elm.Syntax.Node.empty
                            }
                                |> Elm.Syntax.Node.empty
                                |> Just
                        , declaration =
                            { name = "unused" |> Elm.Syntax.Node.empty
                            , arguments = []
                            , expression = codeSnippetCheck.checkedExpression |> Elm.Syntax.Node.empty
                            }
                                |> Elm.Syntax.Node.empty
                        }
                    ]
                    (Elm.CodeGen.fqConstruct [ "Expect" ] "pass" [])


declarationListVisitor : List (Node Elm.Syntax.Declaration.Declaration) -> (NonTestModuleContext -> NonTestModuleContext)
declarationListVisitor declarations =
    \context ->
        case context.exposingKind of
            ExposingExplicit ->
                { context
                    | exposedChoiceTypesExposingVariants =
                        declarations
                            |> List.filterMap
                                (\(Node _ declaration) ->
                                    case declaration of
                                        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                            let
                                                declaredName : String
                                                declaredName =
                                                    choiceTypeDeclaration.name |> Elm.Syntax.Node.value
                                            in
                                            case context.exposedChoiceTypesExposingVariants |> FastDict.get declaredName of
                                                Nothing ->
                                                    Nothing

                                                Just _ ->
                                                    ( declaredName
                                                    , choiceTypeDeclaration.constructors
                                                        |> List.map (\(Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
                                                        |> Set.fromList
                                                    )
                                                        |> Just

                                        _ ->
                                            Nothing
                                )
                            |> FastDict.fromList
                }

            ExposingAll ->
                { context
                    | exposedChoiceTypesExposingVariants =
                        declarations
                            |> List.filterMap
                                (\(Node _ declaration) ->
                                    case declaration of
                                        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                            let
                                                declaredName : String
                                                declaredName =
                                                    choiceTypeDeclaration.name |> Elm.Syntax.Node.value
                                            in
                                            case context.exposedChoiceTypesExposingVariants |> FastDict.get declaredName of
                                                Just _ ->
                                                    Nothing

                                                Nothing ->
                                                    ( declaredName
                                                    , choiceTypeDeclaration.constructors
                                                        |> List.map (\(Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
                                                        |> Set.fromList
                                                    )
                                                        |> Just

                                        _ ->
                                            Nothing
                                )
                            |> FastDict.fromList
                    , exposedValueAndFunctionAndTypeAliasNames =
                        declarations
                            |> List.filterMap
                                (\(Node _ declaration) ->
                                    case declaration of
                                        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                            Nothing

                                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                                            Nothing

                                        Elm.Syntax.Declaration.Destructuring _ _ ->
                                            Nothing

                                        Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                            valueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .name |> Elm.Syntax.Node.value |> Just

                                        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                                            typeAliasDeclaration.name |> Elm.Syntax.Node.value |> Just

                                        Elm.Syntax.Declaration.PortDeclaration signature ->
                                            signature.name |> Elm.Syntax.Node.value |> Just
                                )
                            |> Set.fromList
                }


moduleDocsValueAndFunctionAndTypeAliasNames : Elm.Docs.Module -> Set String
moduleDocsValueAndFunctionAndTypeAliasNames =
    \docsModule ->
        [ docsModule.aliases |> List.map .name
        , docsModule.values |> List.map .name
        , docsModule.unions
            |> List.filterMap
                (\choiceType ->
                    case choiceType.tags of
                        [] ->
                            choiceType.name |> Just

                        _ :: _ ->
                            Nothing
                )
        ]
            |> List.concat
            |> Set.fromList


markdownElmCodeBlocksInModule : String -> List String
markdownElmCodeBlocksInModule =
    \readmeString ->
        case readmeString |> RoughMarkdown.parse of
            Err _ ->
                let
                    _ =
                        Debug.log "rough markdown parsing failed" ()
                in
                []

            Ok markdownBlocks ->
                markdownBlocks
                    |> RoughMarkdown.foldl
                        (\markdownBlock soFar ->
                            case markdownBlock of
                                RoughMarkdown.UnorderedList _ _ ->
                                    soFar

                                RoughMarkdown.OrderedList _ _ _ ->
                                    soFar

                                RoughMarkdown.Paragraph _ ->
                                    soFar

                                RoughMarkdown.CodeBlock codeBlock ->
                                    case codeBlock.language of
                                        Nothing ->
                                            soFar |> (::) codeBlock.body

                                        Just "elm" ->
                                            soFar |> (::) codeBlock.body

                                        Just "" ->
                                            soFar |> (::) codeBlock.body

                                        Just _ ->
                                            soFar
                        )
                        []
                    |> List.reverse


markdownElmCodeBlocksInReadme : String -> List String
markdownElmCodeBlocksInReadme =
    \documentationString ->
        case documentationString |> RoughMarkdown.parse of
            Err _ ->
                let
                    _ =
                        Debug.log "rough markdown parsing failed" ()
                in
                []

            Ok markdownBlocks ->
                markdownBlocks
                    |> RoughMarkdown.foldl
                        (\markdownBlock soFar ->
                            case markdownBlock of
                                RoughMarkdown.UnorderedList _ _ ->
                                    soFar

                                RoughMarkdown.OrderedList _ _ _ ->
                                    soFar

                                RoughMarkdown.Paragraph _ ->
                                    soFar

                                RoughMarkdown.CodeBlock codeBlock ->
                                    case codeBlock.language of
                                        Nothing ->
                                            soFar

                                        Just "elm" ->
                                            soFar |> (::) codeBlock.body

                                        Just _ ->
                                            soFar
                        )
                        []
                    |> List.reverse


initialProjectContext : ProjectContext
initialProjectContext =
    { documentationCodeSnippetTestModuleKey = Nothing
    , byModule = FastDict.empty
    , foundDocumentationExamplesInReadme = []
    }


projectToModuleContextCreator : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContextCreator =
    Review.Rule.initContextCreator
        (\moduleKey moduleName fullAst _ ->
            case moduleName of
                [ "DocumentationCodeSnippet", "Test" ] ->
                    moduleKey |> TestModuleContext

                _ ->
                    let
                        exposingInfo : { kind : ExposingKind, exposedValueAndFunctionAndTypeAliasNames : Set String, exposedChoiceTypesExposingVariants : Set String }
                        exposingInfo =
                            fullAst.moduleDefinition |> Elm.Syntax.Node.value |> moduleHeaderExposed
                    in
                    { exposingKind = exposingInfo.kind
                    , exposedChoiceTypesExposingVariants =
                        exposingInfo.exposedChoiceTypesExposingVariants
                            |> Set.toList
                            |> List.map (\typeName -> ( typeName, Set.empty ))
                            |> FastDict.fromList
                    , exposedValueAndFunctionAndTypeAliasNames = exposingInfo.exposedValueAndFunctionAndTypeAliasNames
                    , foundDocumentationExamplesInModuleHeader = []
                    , foundDocumentationExamplesInMembers = FastDict.empty
                    }
                        |> NonTestModuleContext
        )
        |> Review.Rule.withModuleKey
        |> Review.Rule.withModuleName
        |> Review.Rule.withFullAst


projectContextsMerge : ProjectContext -> ProjectContext -> ProjectContext
projectContextsMerge a b =
    { documentationCodeSnippetTestModuleKey =
        case a.documentationCodeSnippetTestModuleKey of
            Just documentationCodeSnippetTestModuleKey ->
                documentationCodeSnippetTestModuleKey |> Just

            Nothing ->
                b.documentationCodeSnippetTestModuleKey
    , byModule = FastDict.union a.byModule b.byModule
    , foundDocumentationExamplesInReadme = a.foundDocumentationExamplesInReadme
    }


moduleToProjectContextCreator : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContextCreator =
    Review.Rule.initContextCreator
        (\moduleName moduleContext ->
            case moduleContext of
                TestModuleContext testModuleKey ->
                    { documentationCodeSnippetTestModuleKey = testModuleKey |> Just
                    , byModule = FastDict.empty
                    , foundDocumentationExamplesInReadme = []
                    }

                NonTestModuleContext nonTestModuleContext ->
                    { documentationCodeSnippetTestModuleKey = Nothing
                    , byModule =
                        FastDict.singleton moduleName
                            { exposedChoiceTypesExposingVariants = nonTestModuleContext.exposedChoiceTypesExposingVariants
                            , exposedValueAndFunctionAndTypeAliasNames = nonTestModuleContext.exposedValueAndFunctionAndTypeAliasNames
                            , foundDocumentationExamplesInModuleHeader = nonTestModuleContext.foundDocumentationExamplesInModuleHeader
                            , foundDocumentationExamplesInMembers = nonTestModuleContext.foundDocumentationExamplesInMembers
                            }
                    , foundDocumentationExamplesInReadme = []
                    }
        )
        |> Review.Rule.withModuleName


declarationToDocumented : Declaration -> Maybe { name : String, documentation : String }
declarationToDocumented declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
            valueOrFunctionDeclaration.documentation
                |> Maybe.map
                    (\documentation ->
                        { name = valueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .name |> Elm.Syntax.Node.value
                        , documentation = documentation |> Elm.Syntax.Node.value
                        }
                    )

        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
            typeAliasDeclaration.documentation
                |> Maybe.map
                    (\documentation ->
                        { name = typeAliasDeclaration.name |> Elm.Syntax.Node.value
                        , documentation = documentation |> Elm.Syntax.Node.value
                        }
                    )

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            choiceTypeDeclaration.documentation
                |> Maybe.map
                    (\documentation ->
                        { name = choiceTypeDeclaration.name |> Elm.Syntax.Node.value
                        , documentation = documentation |> Elm.Syntax.Node.value
                        }
                    )

        Elm.Syntax.Declaration.PortDeclaration _ ->
            -- not supported
            Nothing

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            -- not supported
            Nothing

        Elm.Syntax.Declaration.Destructuring _ _ ->
            -- impossible
            Nothing


elmCodeBlockToSnippet : String -> Maybe CodeSnippet
elmCodeBlockToSnippet =
    \elmCode ->
        let
            split : { withoutChecks : String, checks : List CodeSnippetCheck }
            split =
                elmCode |> elmCodeBlockLinesSplitOffChecks
        in
        case split.withoutChecks |> ElmSyntaxParse.importsAndDeclarations of
            Nothing ->
                let
                    _ =
                        Debug.log "imports and declarations parsing failed" split.withoutChecks
                in
                Nothing

            Just importsAndDeclarations ->
                { imports = importsAndDeclarations.imports
                , declarations = importsAndDeclarations.declarations
                , checks = split.checks
                }
                    |> Just


{-| A chunk is opened by an unindented line and only closed by a blank line
-}
codeBlockToChunks : String -> List String
codeBlockToChunks =
    \codeBlock ->
        codeBlock
            |> String.split "\n"
            |> List.foldl
                (\line soFar ->
                    case line |> String.uncons of
                        Nothing ->
                            { lastLineWasBlank = True
                            , chunks = soFar.chunks
                            }

                        Just ( lineFirstChar, lineCharsAfterFirst ) ->
                            { lastLineWasBlank = False
                            , chunks =
                                case soFar.chunks of
                                    [] ->
                                        case lineFirstChar of
                                            ' ' ->
                                                []

                                            nonSpaceFirstChar ->
                                                [ { lineBeforeIsUnindented = True, content = String.cons nonSpaceFirstChar lineCharsAfterFirst } ]

                                    currentChunk :: blocksBeforeCurrentChunk ->
                                        case lineFirstChar of
                                            ' ' ->
                                                { lineBeforeIsUnindented = False
                                                , content = [ currentChunk.content, "\n ", lineCharsAfterFirst ] |> String.concat
                                                }
                                                    :: blocksBeforeCurrentChunk

                                            lineFirstCharNonSpace ->
                                                if currentChunk.lineBeforeIsUnindented then
                                                    { lineBeforeIsUnindented = True
                                                    , content = [ currentChunk.content, "\n", String.cons lineFirstCharNonSpace lineCharsAfterFirst ] |> String.concat
                                                    }
                                                        :: blocksBeforeCurrentChunk

                                                else
                                                    { lineBeforeIsUnindented = True, content = String.cons lineFirstCharNonSpace lineCharsAfterFirst }
                                                        :: currentChunk
                                                        :: blocksBeforeCurrentChunk
                            }
                )
                { chunks = [], lastLineWasBlank = True }
            |> .chunks
            |> List.reverse
            |> List.map .content


elmCodeBlockLinesSplitOffChecks : String -> { withoutChecks : String, checks : List CodeSnippetCheck }
elmCodeBlockLinesSplitOffChecks =
    \elmCodeBlock ->
        let
            chunkSplitAt : String -> (String -> List String)
            chunkSplitAt mark =
                \chunk ->
                    case chunk |> String.split mark of
                        -- can't happen
                        [] ->
                            [ chunk ]

                        [ onlyChunk ] ->
                            [ onlyChunk ]

                        realChunk0 :: realChunk1 :: realChunk2Up ->
                            [ realChunk0, mark ++ ((realChunk1 :: realChunk2Up) |> String.join mark) ]
        in
        elmCodeBlock
            |> codeBlockToChunks
            |> List.concatMap (chunkSplitAt "-->")
            |> List.concatMap (chunkSplitAt "--:")
            |> elmCodeBlockChunksSplitOffChecks


elmCodeBlockChunksSplitOffChecks : List String -> { withoutChecks : String, checks : List CodeSnippetCheck }
elmCodeBlockChunksSplitOffChecks chunks =
    case chunks of
        [] ->
            { withoutChecks = "", checks = [] }

        [ onlyChunk ] ->
            { withoutChecks = onlyChunk, checks = [] }

        chunk0 :: chunk1 :: chunk2Up ->
            case chunk0 |> ElmSyntaxParse.expression of
                Nothing ->
                    let
                        chunk1Up : { withoutChecks : String, checks : List CodeSnippetCheck }
                        chunk1Up =
                            (chunk1 :: chunk2Up) |> elmCodeBlockChunksSplitOffChecks
                    in
                    { chunk1Up | withoutChecks = [ chunk0, "\n\n", chunk1Up.withoutChecks ] |> String.concat }

                Just checked ->
                    let
                        chunk2UpSeparated : { withoutChecks : String, checks : List CodeSnippetCheck }
                        chunk2UpSeparated =
                            chunk2Up |> elmCodeBlockChunksSplitOffChecks
                    in
                    case chunk1 |> toMarked "-->" of
                        Just expectedExpressionSource ->
                            case expectedExpressionSource |> ElmSyntaxParse.expression of
                                Nothing ->
                                    -- not a check
                                    { chunk2UpSeparated | withoutChecks = [ chunk0, "\n\n", chunk1, "\n\n", chunk2UpSeparated.withoutChecks ] |> String.concat }

                                Just expectedExpression ->
                                    { chunk2UpSeparated
                                        | checks =
                                            chunk2UpSeparated.checks
                                                |> (::) { checkedExpression = checked, expectation = Equals expectedExpression }
                                    }

                        Nothing ->
                            case chunk1 |> toMarked "--:" of
                                Just expectedTypeSource ->
                                    case expectedTypeSource |> ElmSyntaxParse.type_ of
                                        Nothing ->
                                            -- not a check
                                            { chunk2UpSeparated | withoutChecks = [ chunk0, "\n\n", chunk1, "\n\n", chunk2UpSeparated.withoutChecks ] |> String.concat }

                                        Just expectedType ->
                                            { chunk2UpSeparated
                                                | checks =
                                                    chunk2UpSeparated.checks
                                                        |> (::) { checkedExpression = checked, expectation = IsOfType expectedType }
                                            }

                                Nothing ->
                                    -- not a check
                                    { chunk2UpSeparated | withoutChecks = [ chunk0, "\n\n", chunk1, "\n\n", chunk2UpSeparated.withoutChecks ] |> String.concat }


toMarked : String -> (String -> Maybe String)
toMarked mark =
    \string ->
        case string |> String.split "\n" |> List.LocalExtra.allJustMap (stringToWithoutStart mark) of
            Just linesWithoutStart ->
                linesWithoutStart |> String.join "\n" |> Just

            Nothing ->
                case string |> stringToWithoutStart mark of
                    Just linesWithoutStart ->
                        linesWithoutStart |> Just

                    Nothing ->
                        Nothing


stringToWithoutStart : String -> (String -> Maybe String)
stringToWithoutStart start =
    \string ->
        if string |> String.startsWith start then
            string
                |> String.dropLeft (start |> String.length)
                |> String.trimLeft
                |> Just

        else
            Nothing
