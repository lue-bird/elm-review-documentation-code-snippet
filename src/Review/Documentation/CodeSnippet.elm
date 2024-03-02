module Review.Documentation.CodeSnippet exposing (check)

{-| Checks your small code examples in the readme, module headers and declaration comments
for valid syntax, matching types and correctness
by generating tests from these code snippets.

    import Review.Documentation.CodeSnippet

    config =
        [ Review.Documentation.CodeSnippet.check
        ]

@docs check

-}

import Declaration.LocalExtra
import Dict
import Elm.CodeGen
import Elm.DSLParser
import Elm.Docs
import Elm.Pretty
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation
import ElmSyntaxParse
import Expression.LocalExtra
import FastDict exposing (Dict)
import Imports exposing (Imports)
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
    { documentationCodeSnippetTestModule : Maybe { key : ModuleKey, source : String }
    , exposesByModule :
        Dict
            Elm.Syntax.ModuleName.ModuleName
            { exposedChoiceTypesExposingVariants : Dict String (Set String)
            , exposedValueAndFunctionAndTypeAliasNames : Set String
            }
    , codeSnippetsByModule :
        Dict
            Elm.Syntax.ModuleName.ModuleName
            { key : Review.Rule.ModuleKey
            , inModuleHeader : List CodeSnippet
            , inMembers : Dict String (List CodeSnippet)
            }
    , readmeCodeSnippets : List CodeSnippet
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
        { extractSourceCode : Range -> String
        , exposingKind : ExposingKind
        , exposedValueAndFunctionAndTypeAliasNames : Set String
        , exposedChoiceTypesExposingVariants : Dict String (Set String)
        , headerCodeSnippets : List CodeSnippet
        , codeSnippetsByMember : Dict String (List CodeSnippet)
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


{-| [`Rule`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#Rule)
to generate tests from your documentation examples (readme, module header, declaration comment)
and also to report syntax errors for these code snippets. There are two kinds of checks:


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

**important** The rule will only report syntax and type errors
if your code snippet contains at least one of these two checks, so replace

    {-| ...

        myResult : Cmd Int
        myResult =
            Ok 3 |> Cmd.Extra.fromResult

    -}

with

    {-| ...

        Ok 3 |> Cmd.Extra.fromResult
        --: Cmd Int

    -}

Both checks will be run on _all modules_ and the readme. If you want to disable this for e.g. generated or vendored code,
use [`Review.Rule.ignoreErrorsForDirectories`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/Review-Rule#ignoreErrorsForDirectories)

-}
check : Rule
check =
    Review.Rule.newProjectRuleSchema "Review.Documentation.CodeSnippet.check" initialProjectContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withReadmeProjectVisitor
            (\maybeReadme context ->
                case maybeReadme of
                    Nothing ->
                        ( [], context )

                    Just readme ->
                        let
                            codeSnippetsAndErrors : List (Result CodeSnippetParseError CodeSnippet)
                            codeSnippetsAndErrors =
                                readme.content |> markdownElmCodeBlocksInReadme |> List.filterMap elmCodeBlockToSnippet
                        in
                        ( codeSnippetsAndErrors
                            |> List.filterMap
                                (\result ->
                                    case result of
                                        Err error ->
                                            error |> Just

                                        Ok _ ->
                                            Nothing
                                )
                            |> List.map
                                (\error ->
                                    Review.Rule.errorForReadme
                                        readme.readmeKey
                                        (error |> codeSnippetParseErrorInfo)
                                        (error
                                            |> codeSnippetParseErrorRangeIn
                                                { raw = readme.content, start = { row = 1, column = 1 } }
                                        )
                                )
                        , { context
                            | readmeCodeSnippets =
                                codeSnippetsAndErrors |> List.filterMap Result.toMaybe
                          }
                        )
            )
        |> Review.Rule.withDirectDependenciesProjectVisitor
            (\dependencyDocsDict context ->
                ( []
                , { context
                    | exposesByModule =
                        FastDict.union context.exposesByModule
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
                    |> Review.Rule.withCommentsVisitor
                        (\comments context ->
                            case context of
                                TestModuleContext testModuleContext ->
                                    ( [], testModuleContext |> TestModuleContext )

                                NonTestModuleContext nonTestModuleContext ->
                                    case comments |> List.LocalExtra.firstJustMap (commentToModuleHeader nonTestModuleContext) of
                                        -- no module header documentation
                                        Nothing ->
                                            ( [], nonTestModuleContext |> NonTestModuleContext )

                                        Just moduleHeaderDocumentation ->
                                            let
                                                codeSnippetsAndErrors : List (Result CodeSnippetParseError CodeSnippet)
                                                codeSnippetsAndErrors =
                                                    moduleHeaderDocumentation |> Elm.Syntax.Node.value |> markdownElmCodeBlocksInModule |> List.filterMap elmCodeBlockToSnippet
                                            in
                                            ( codeSnippetsAndErrors
                                                |> List.filterMap
                                                    (\result ->
                                                        case result of
                                                            Err error ->
                                                                error |> Just

                                                            Ok _ ->
                                                                Nothing
                                                    )
                                                |> List.map
                                                    (\error ->
                                                        Review.Rule.error
                                                            (error |> codeSnippetParseErrorInfo)
                                                            (error
                                                                |> codeSnippetParseErrorRangeIn
                                                                    { raw = moduleHeaderDocumentation |> Elm.Syntax.Node.value
                                                                    , start = moduleHeaderDocumentation |> Elm.Syntax.Node.range |> .start
                                                                    }
                                                            )
                                                    )
                                            , { nonTestModuleContext
                                                | headerCodeSnippets =
                                                    codeSnippetsAndErrors |> List.filterMap Result.toMaybe
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
                            case context of
                                TestModuleContext testModuleContext ->
                                    ( [], testModuleContext |> TestModuleContext )

                                NonTestModuleContext nonTestModuleContext ->
                                    case declaration |> declarationToDocumented of
                                        Nothing ->
                                            ( [], nonTestModuleContext |> NonTestModuleContext )

                                        Just memberDocumented ->
                                            let
                                                codeSnippetsAndErrors : List (Result CodeSnippetParseError CodeSnippet)
                                                codeSnippetsAndErrors =
                                                    memberDocumented.documentation |> Elm.Syntax.Node.value |> markdownElmCodeBlocksInModule |> List.filterMap elmCodeBlockToSnippet
                                            in
                                            ( codeSnippetsAndErrors
                                                |> List.filterMap
                                                    (\result ->
                                                        case result of
                                                            Err error ->
                                                                error |> Just

                                                            Ok _ ->
                                                                Nothing
                                                    )
                                                |> List.map
                                                    (\error ->
                                                        Review.Rule.error
                                                            (error |> codeSnippetParseErrorInfo)
                                                            (error
                                                                |> codeSnippetParseErrorRangeIn
                                                                    { raw = memberDocumented.documentation |> Elm.Syntax.Node.value
                                                                    , start = memberDocumented.documentation |> Elm.Syntax.Node.range |> .start
                                                                    }
                                                            )
                                                    )
                                            , { nonTestModuleContext
                                                | codeSnippetsByMember =
                                                    nonTestModuleContext.codeSnippetsByMember
                                                        |> FastDict.insert memberDocumented.name
                                                            (codeSnippetsAndErrors |> List.filterMap Result.toMaybe)
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


commentToModuleHeader : { resources_ | extractSourceCode : Range -> String } -> (Node String -> Maybe (Node String))
commentToModuleHeader resources =
    \(Node commentRange comment) ->
        if comment |> String.startsWith "{-|" then
            case
                { start = { row = commentRange.end.row + 1, column = 1 }
                , end = { row = commentRange.end.row + 1, column = 4 }
                }
                    |> resources.extractSourceCode
            of
                "port" ->
                    Nothing

                _ ->
                    Just (Elm.Syntax.Node.Node commentRange comment)

        else
            Nothing


everythingRange : Range
everythingRange =
    { start = { row = 1, column = 1 }, end = { row = 1000000, column = 1 } }


checkFullProject : ProjectContext -> List (Review.Rule.Error { useErrorForModule : () })
checkFullProject =
    \context ->
        case context.documentationCodeSnippetTestModule of
            Nothing ->
                [ Review.Rule.globalError
                    { message = "documentation code snippet test module needs to be added"
                    , details =
                        [ "We need a module to generate documentation code snippet tests in. Please add a module tests/DocumentationCodeSnippetTest.elm."
                        ]
                    }
                ]

            Just documentationCodeSnippetTestModule ->
                let
                    generatedTestFile : String
                    generatedTestFile =
                        { readmeCodeSnippets = context.readmeCodeSnippets
                        , exposesByModule = context.exposesByModule
                        , codeSnippetsByModule = context.codeSnippetsByModule
                        }
                            |> createDocumentationCodeSnippetsTestFile
                            |> Elm.Pretty.pretty 80

                    testFileRequiresChanges : Bool
                    testFileRequiresChanges =
                        -- parse and format the existing test module, then compare it with the generated one.
                        -- This is somewhat roundabout but I wasn't able to find something simple that's nicer
                        case documentationCodeSnippetTestModule.source |> Elm.DSLParser.parse of
                            Err _ ->
                                True

                            Ok existingFile ->
                                (existingFile |> Elm.Pretty.pretty 80) /= generatedTestFile
                in
                if testFileRequiresChanges then
                    [ Review.Rule.errorForModuleWithFix documentationCodeSnippetTestModule.key
                        { message = "documentation code snippet test can be added"
                        , details =
                            [ "Adding them will help verify that code blocks in your readme and module documentation work correctly."
                            ]
                        }
                        { start = { row = 1, column = 1 }, end = { row = 1, column = 7 } }
                        [ Review.Fix.replaceRangeBy
                            everythingRange
                            generatedTestFile
                        ]
                    ]

                else
                    []


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


createDocumentationCodeSnippetsTestFile :
    { readmeCodeSnippets : List CodeSnippet
    , exposesByModule :
        Dict
            Elm.Syntax.ModuleName.ModuleName
            { exposedChoiceTypesExposingVariants : Dict String (Set String)
            , exposedValueAndFunctionAndTypeAliasNames : Set String
            }
    , codeSnippetsByModule :
        Dict
            Elm.Syntax.ModuleName.ModuleName
            { key : Review.Rule.ModuleKey
            , inModuleHeader : List CodeSnippet
            , inMembers : Dict String (List CodeSnippet)
            }
    }
    -> Elm.CodeGen.File
createDocumentationCodeSnippetsTestFile =
    \infoRaw ->
        let
            {-
               - suffix declaration names based on #location
               - map all sub references (type, pattern, value/function) of declarations and actual and expected
                 - to fully qualified using Origin.determine
                 - if no full qualification is found, see if it's defined in `declarations` and adapt the #location name accordingly
            -}
            codeSnippetFullyQualifyAndAddLocationSuffix : String -> (CodeSnippet -> CodeSnippet)
            codeSnippetFullyQualifyAndAddLocationSuffix locationSuffix =
                \codeSnippet ->
                    let
                        imports : Imports
                        imports =
                            Imports.implicit
                                |> Imports.insertSyntaxImports infoRaw.exposesByModule codeSnippet.imports

                        snippetLocalDeclarationNames : Set String
                        snippetLocalDeclarationNames =
                            codeSnippet.declarations
                                |> List.LocalExtra.setUnionMap Declaration.LocalExtra.names

                        referenceFullyQualifyAndAdaptLocationSuffix : ( Elm.Syntax.ModuleName.ModuleName, String ) -> ( Elm.Syntax.ModuleName.ModuleName, String )
                        referenceFullyQualifyAndAdaptLocationSuffix =
                            \( qualification, unqualifiedName ) ->
                                case ( qualification, unqualifiedName ) |> Origin.determine imports of
                                    [] ->
                                        if snippetLocalDeclarationNames |> Set.member unqualifiedName then
                                            ( [], [ unqualifiedName, "__", locationSuffix ] |> String.concat )

                                        else
                                            -- pattern variable or let
                                            ( [], unqualifiedName )

                                    moduleNamePart0 :: moduleNamePart1Up ->
                                        ( moduleNamePart0 :: moduleNamePart1Up, unqualifiedName )

                        codeSnippetCheckFullyQualify : CodeSnippetCheck -> CodeSnippetCheck
                        codeSnippetCheckFullyQualify =
                            \codeSnippetCheck ->
                                { checkedExpression =
                                    codeSnippetCheck.checkedExpression
                                        |> Expression.LocalExtra.referencesAlter referenceFullyQualifyAndAdaptLocationSuffix
                                , expectation =
                                    case codeSnippetCheck.expectation of
                                        Equals expectedExpression ->
                                            expectedExpression
                                                |> Expression.LocalExtra.referencesAlter referenceFullyQualifyAndAdaptLocationSuffix
                                                |> Equals

                                        IsOfType expectedType ->
                                            expectedType
                                                |> Type.LocalExtra.referencesAlter referenceFullyQualifyAndAdaptLocationSuffix
                                                |> IsOfType
                                }
                    in
                    { imports = codeSnippet.imports
                    , declarations =
                        codeSnippet.declarations
                            |> List.map
                                (\declaration ->
                                    declaration
                                        |> Declaration.LocalExtra.nameAlter (\name -> [ name, "__", locationSuffix ] |> String.concat)
                                        |> Declaration.LocalExtra.subReferencesAlter referenceFullyQualifyAndAdaptLocationSuffix
                                )
                    , checks = codeSnippet.checks |> List.map codeSnippetCheckFullyQualify
                    }

            codeSnippetsFullyQualifyAndAddLocationSuffix : String -> (List CodeSnippet -> List CodeSnippet)
            codeSnippetsFullyQualifyAndAddLocationSuffix locationSuffix =
                \codeSnippetsRaw ->
                    codeSnippetsRaw
                        |> List.indexedMap
                            (\snippetIndex codeSnippet ->
                                codeSnippet
                                    |> codeSnippetFullyQualifyAndAddLocationSuffix
                                        ([ locationSuffix, "_", snippetIndex |> String.fromInt ] |> String.concat)
                            )

            readmeCodeSnippets : List CodeSnippet
            readmeCodeSnippets =
                infoRaw.readmeCodeSnippets |> codeSnippetsFullyQualifyAndAddLocationSuffix "Readme"

            codeSnippetsByModule :
                Dict
                    Elm.Syntax.ModuleName.ModuleName
                    { key : Review.Rule.ModuleKey
                    , inModuleHeader : List CodeSnippet
                    , inMembers : Dict String (List CodeSnippet)
                    }
            codeSnippetsByModule =
                infoRaw.codeSnippetsByModule
                    |> FastDict.map
                        (\moduleName moduleInfoRaw ->
                            let
                                moduleNameLocationSuffix : String -> String
                                moduleNameLocationSuffix subName =
                                    [ moduleName |> String.join "_", "__", subName ] |> String.concat

                                codeSnippetAddImportLocalModuleExposingAll : CodeSnippet -> CodeSnippet
                                codeSnippetAddImportLocalModuleExposingAll =
                                    \codeSnippet ->
                                        { codeSnippet
                                            | imports =
                                                Elm.CodeGen.importStmt moduleName Nothing (Elm.CodeGen.exposeAll |> Just)
                                                    :: codeSnippet.imports
                                        }
                            in
                            { key = moduleInfoRaw.key
                            , inModuleHeader =
                                moduleInfoRaw.inModuleHeader
                                    |> List.map codeSnippetAddImportLocalModuleExposingAll
                                    |> codeSnippetsFullyQualifyAndAddLocationSuffix (moduleNameLocationSuffix "Header")
                            , inMembers =
                                moduleInfoRaw.inMembers
                                    |> FastDict.map
                                        (\memberName memberCodeSnippets ->
                                            memberCodeSnippets
                                                |> List.map codeSnippetAddImportLocalModuleExposingAll
                                                |> codeSnippetsFullyQualifyAndAddLocationSuffix (moduleNameLocationSuffix memberName)
                                        )
                            }
                        )

            codeSnippetTests : List Elm.CodeGen.Expression
            codeSnippetTests =
                (readmeCodeSnippets |> codeSnippetsToTestWithName "readme")
                    :: (codeSnippetsByModule
                            |> FastDict.toList
                            |> List.map
                                (\( moduleName, moduleContext ) ->
                                    elmCodeGenTestDescribe (moduleName |> String.join ".")
                                        ((moduleContext
                                            |> .inModuleHeader
                                            |> codeSnippetsToTestWithName "module header"
                                         )
                                            :: (moduleContext
                                                    |> .inMembers
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
                readmeCodeSnippets
                    ++ (codeSnippetsByModule
                            |> FastDict.values
                            |> List.concatMap
                                (\moduleContext ->
                                    moduleContext.inModuleHeader
                                        ++ (moduleContext
                                                |> .inMembers
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
            (Elm.CodeGen.normalModule [ "DocumentationCodeSnippetTest" ] [ Elm.CodeGen.funExpose "tests" ])
            (Set.diff
                (codeSnippets
                    |> List.LocalExtra.setUnionMap codeSnippetUsedModules
                    |> Set.insert [ "Expect" ]
                    |> Set.insert [ "Test" ]
                )
                (Imports.implicit
                    |> FastDict.toList
                    |> List.filterMap
                        (\( moduleName, implicitImport ) ->
                            case implicitImport.alias of
                                Just _ ->
                                    Nothing

                                Nothing ->
                                    moduleName |> Just
                        )
                    |> Set.fromList
                )
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
                :: (codeSnippets
                        |> List.concatMap .declarations
                        |> List.map Elm.CodeGen.DeclNoComment
                   )
            )
            (Elm.CodeGen.emptyFileComment
                |> Elm.CodeGen.markdown "automatically generated by [elm-review-documentation-code-snippet](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-documentation-code-snippet/latest)"
                |> Just
            )


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
        readmeString
            |> RoughMarkdown.parse
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
        documentationString
            |> RoughMarkdown.parse
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
    { documentationCodeSnippetTestModule = Nothing
    , exposesByModule = FastDict.empty
    , codeSnippetsByModule = FastDict.empty
    , readmeCodeSnippets = []
    }


projectToModuleContextCreator : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContextCreator =
    Review.Rule.initContextCreator
        (\moduleKey moduleName extractSourceCode fullAst _ ->
            case moduleName of
                [ "DocumentationCodeSnippetTest" ] ->
                    moduleKey |> TestModuleContext

                _ ->
                    let
                        exposingInfo : { kind : ExposingKind, exposedValueAndFunctionAndTypeAliasNames : Set String, exposedChoiceTypesExposingVariants : Set String }
                        exposingInfo =
                            fullAst.moduleDefinition |> Elm.Syntax.Node.value |> moduleHeaderExposed
                    in
                    { extractSourceCode = extractSourceCode
                    , exposingKind = exposingInfo.kind
                    , exposedChoiceTypesExposingVariants =
                        exposingInfo.exposedChoiceTypesExposingVariants
                            |> Set.toList
                            |> List.map (\typeName -> ( typeName, Set.empty ))
                            |> FastDict.fromList
                    , exposedValueAndFunctionAndTypeAliasNames = exposingInfo.exposedValueAndFunctionAndTypeAliasNames
                    , headerCodeSnippets = []
                    , codeSnippetsByMember = FastDict.empty
                    }
                        |> NonTestModuleContext
        )
        |> Review.Rule.withModuleKey
        |> Review.Rule.withModuleName
        |> Review.Rule.withSourceCodeExtractor
        |> Review.Rule.withFullAst


projectContextsMerge : ProjectContext -> ProjectContext -> ProjectContext
projectContextsMerge a b =
    { documentationCodeSnippetTestModule =
        case a.documentationCodeSnippetTestModule of
            Just documentationCodeSnippetTestModuleKey ->
                documentationCodeSnippetTestModuleKey |> Just

            Nothing ->
                b.documentationCodeSnippetTestModule
    , exposesByModule = FastDict.union a.exposesByModule b.exposesByModule
    , codeSnippetsByModule = FastDict.union a.codeSnippetsByModule b.codeSnippetsByModule
    , readmeCodeSnippets = a.readmeCodeSnippets ++ b.readmeCodeSnippets
    }


moduleToProjectContextCreator : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContextCreator =
    Review.Rule.initContextCreator
        (\moduleKey moduleName extractSourceCode moduleContext ->
            case moduleContext of
                TestModuleContext testModuleKey ->
                    { documentationCodeSnippetTestModule =
                        { key = testModuleKey, source = extractSourceCode everythingRange } |> Just
                    , exposesByModule = FastDict.empty
                    , codeSnippetsByModule = FastDict.empty
                    , readmeCodeSnippets = []
                    }

                NonTestModuleContext nonTestModuleContext ->
                    { documentationCodeSnippetTestModule = Nothing
                    , exposesByModule =
                        FastDict.singleton moduleName
                            { exposedChoiceTypesExposingVariants = nonTestModuleContext.exposedChoiceTypesExposingVariants
                            , exposedValueAndFunctionAndTypeAliasNames = nonTestModuleContext.exposedValueAndFunctionAndTypeAliasNames
                            }
                    , codeSnippetsByModule =
                        FastDict.singleton moduleName
                            { key = moduleKey
                            , inModuleHeader = nonTestModuleContext.headerCodeSnippets
                            , inMembers = nonTestModuleContext.codeSnippetsByMember
                            }
                    , readmeCodeSnippets = []
                    }
        )
        |> Review.Rule.withModuleKey
        |> Review.Rule.withModuleName
        |> Review.Rule.withSourceCodeExtractor


declarationToDocumented : Declaration -> Maybe { name : String, documentation : Node String }
declarationToDocumented declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
            valueOrFunctionDeclaration.documentation
                |> Maybe.map
                    (\documentation ->
                        { name = valueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .name |> Elm.Syntax.Node.value
                        , documentation = documentation
                        }
                    )

        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
            typeAliasDeclaration.documentation
                |> Maybe.map
                    (\documentation ->
                        { name = typeAliasDeclaration.name |> Elm.Syntax.Node.value
                        , documentation = documentation
                        }
                    )

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
            choiceTypeDeclaration.documentation
                |> Maybe.map
                    (\documentation ->
                        { name = choiceTypeDeclaration.name |> Elm.Syntax.Node.value
                        , documentation = documentation
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


codeSnippetParseErrorInfo : CodeSnippetParseError -> { message : String, details : List String }
codeSnippetParseErrorInfo =
    \codeSnippetParseError ->
        case codeSnippetParseError of
            CodeSnippetDeclarationsAndImportsParseError _ ->
                { message = "code snippet parsing failed"
                , details =
                    [ "I expected to find syntactically valid elm code here but something is off with the imports/declarations/checked expressions."
                    , "If you don't see an obvious mistake, try moving the code to an elm module and see where the compiler complains."
                    ]
                }

            CodeSnippetExpectationParseError _ ->
                { message = "code snippet expectation parsing failed"
                , details =
                    [ "I found an expectation marker --> or --:, so I expected a syntactically valid expression or type next."
                    , "If you don't see an obvious mistake, try moving the code to an elm module and see where the compiler complains."
                    ]
                }


rangeFrom : Location -> (Range -> Range)
rangeFrom offsetLocation =
    \range ->
        { start = range.start |> locationFrom offsetLocation
        , end = range.end |> locationFrom offsetLocation
        }


locationFrom : Location -> (Location -> Location)
locationFrom offsetLocation =
    \location ->
        case location.row of
            1 ->
                { row = offsetLocation.row
                , column = offsetLocation.column + location.column - 1
                }

            startRowAtLeast2 ->
                { row = offsetLocation.row + startRowAtLeast2 - 1
                , column = location.column
                }


codeSnippetParseErrorRangeIn : { raw : String, start : Location } -> (CodeSnippetParseError -> Range)
codeSnippetParseErrorRangeIn documentation =
    \parseError ->
        let
            relativeRange : Range
            relativeRange =
                case parseError of
                    CodeSnippetDeclarationsAndImportsParseError fullDeclarationsAndImportsToFind ->
                        documentation.raw
                            |> stringRangeOfString
                                (fullDeclarationsAndImportsToFind
                                    |> String.split "\n"
                                    |> List.head
                                    -- never returned by String.split
                                    |> Maybe.withDefault fullDeclarationsAndImportsToFind
                                )

                    CodeSnippetExpectationParseError toFind ->
                        documentation.raw |> stringRangeOfString (toFind |> String.trimLeft)
        in
        relativeRange |> rangeFrom documentation.start


stringRangeOfString : String -> (String -> Range)
stringRangeOfString toFind =
    \string ->
        case string |> String.split toFind of
            -- never returned by String.split
            [] ->
                { start = { row = 1, column = 1 }, end = { row = 2, column = 1 } }

            beforeToFind :: _ ->
                toFind
                    |> stringRange
                    |> rangeFrom (beforeToFind |> stringRange |> .end)


stringRange : String -> Range
stringRange =
    \string ->
        let
            lines : List String
            lines =
                string |> String.split "\n"
        in
        { start = { row = 1, column = 1 }
        , end =
            case lines |> List.LocalExtra.last of
                Nothing ->
                    { row = 1, column = 1 }

                Just beforeToFindLastLine ->
                    { row = lines |> List.length
                    , column =
                        (beforeToFindLastLine |> String.length) + 1
                    }
        }


elmCodeBlockToSnippet : String -> Maybe (Result CodeSnippetParseError CodeSnippet)
elmCodeBlockToSnippet =
    \elmCode ->
        case elmCode |> elmCodeBlockSplitOffChecks of
            Err toFind ->
                CodeSnippetExpectationParseError toFind |> Err |> Just

            Ok split ->
                case split.checks of
                    [] ->
                        Nothing

                    check0 :: check1Up ->
                        case split.withoutChecks |> ElmSyntaxParse.importsAndDeclarations of
                            Nothing ->
                                CodeSnippetDeclarationsAndImportsParseError split.withoutChecks |> Err |> Just

                            Just importsAndDeclarations ->
                                { imports = importsAndDeclarations.imports
                                , declarations = importsAndDeclarations.declarations
                                , checks = check0 :: check1Up
                                }
                                    |> Ok
                                    |> Just


toMarked : String -> (String -> Maybe String)
toMarked mark =
    \string ->
        case string |> String.split "\n" |> List.LocalExtra.allJustMap (stringToWithoutStart mark) of
            Just linesWithoutStart ->
                linesWithoutStart |> String.join "\n" |> Just

            Nothing ->
                case string |> stringToWithoutStart (mark ++ "\n") of
                    Just linesWithoutStart ->
                        linesWithoutStart |> Just

                    Nothing ->
                        Nothing


stringToWithoutStart : String -> (String -> Maybe String)
stringToWithoutStart start =
    \string ->
        let
            stringTrimmedLeft : String
            stringTrimmedLeft =
                string |> String.trimLeft
        in
        if stringTrimmedLeft |> String.startsWith start then
            stringTrimmedLeft
                |> String.dropLeft (start |> String.length)
                |> Just

        else
            Nothing


elmCodeBlockSplitOffChecks : String -> Result String { withoutChecks : String, checks : List CodeSnippetCheck }
elmCodeBlockSplitOffChecks =
    \elmCodeBlock ->
        let
            chunkSplitAt : String -> (String -> List String)
            chunkSplitAt mark =
                \chunk ->
                    case chunk |> String.split mark of
                        -- can't happen
                        [] ->
                            [ chunk ]

                        -- no mark
                        [ onlyChunk ] ->
                            [ onlyChunk ]

                        -- starts with mark
                        [ "", onlyChunk ] ->
                            [ mark ++ onlyChunk ]

                        -- has content before mark
                        [ realChunk0, realChunk1 ] ->
                            [ realChunk0, mark ++ realChunk1 ]

                        -- multiple marks
                        realChunk0 :: chunk0AfterReal0 :: chunk1AfterReal0 :: chunk2AfterReal0Up ->
                            case ((mark ++ chunk0AfterReal0) :: chunk1AfterReal0 :: chunk2AfterReal0Up) |> String.join mark |> toMarked mark of
                                -- all next lines build one marked
                                Just marked ->
                                    [ realChunk0
                                    , marked |> String.split "\n" |> List.map (\line -> mark ++ line) |> String.join "\n"
                                    ]

                                -- multiple crammed together without blank lines between
                                Nothing ->
                                    case chunk0AfterReal0 |> String.split "\n" of
                                        -- String.split never returns []
                                        [] ->
                                            [ realChunk0, mark ++ ((chunk0AfterReal0 :: chunk1AfterReal0 :: chunk2AfterReal0Up) |> String.join mark) ]

                                        realChunk1 :: afterRealChunk1BeforeChunk1AfterReal0 ->
                                            realChunk0
                                                :: (mark ++ realChunk1)
                                                :: ((afterRealChunk1BeforeChunk1AfterReal0 |> String.join "\n")
                                                        :: (chunk1AfterReal0 :: chunk2AfterReal0Up)
                                                        |> String.join mark
                                                        |> chunkSplitAt mark
                                                   )
        in
        elmCodeBlock
            |> codeBlockToChunks
            |> List.concatMap (chunkSplitAt "-->")
            |> List.concatMap (chunkSplitAt "--:")
            |> List.filterMap
                (\chunk ->
                    case chunk of
                        "" ->
                            Nothing

                        filledChunk ->
                            filledChunk |> Just
                )
            |> elmCodeBlockChunksSplitOffChecks


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
                                                if currentChunk.lineBeforeIsUnindented && not soFar.lastLineWasBlank then
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


elmCodeBlockChunksSplitOffChecks : List String -> Result String { withoutChecks : String, checks : List CodeSnippetCheck }
elmCodeBlockChunksSplitOffChecks chunks =
    -- IGNORE TCO
    case chunks of
        [] ->
            { withoutChecks = "", checks = [] } |> Ok

        [ onlyChunk ] ->
            { withoutChecks = onlyChunk, checks = [] } |> Ok

        chunk0 :: chunk1 :: chunk2Up ->
            case chunk0 |> ElmSyntaxParse.expression of
                Nothing ->
                    (chunk1 :: chunk2Up)
                        |> elmCodeBlockChunksSplitOffChecks
                        |> Result.map
                            (\chunk1Up ->
                                { chunk1Up | withoutChecks = [ chunk0, "\n\n", chunk1Up.withoutChecks ] |> String.concat }
                            )

                Just checked ->
                    chunk2Up
                        |> elmCodeBlockChunksSplitOffChecks
                        |> Result.andThen
                            (\chunk2UpSeparated ->
                                case chunk1 |> toMarked "-->" of
                                    Just expectedExpressionSource ->
                                        case expectedExpressionSource |> ElmSyntaxParse.expression of
                                            Nothing ->
                                                expectedExpressionSource |> Err

                                            Just expectedExpression ->
                                                { chunk2UpSeparated
                                                    | checks =
                                                        chunk2UpSeparated.checks
                                                            |> (::) { checkedExpression = checked, expectation = Equals expectedExpression }
                                                }
                                                    |> Ok

                                    Nothing ->
                                        case chunk1 |> toMarked "--:" of
                                            Just expectedTypeSource ->
                                                case expectedTypeSource |> ElmSyntaxParse.type_ of
                                                    Nothing ->
                                                        expectedTypeSource |> Err

                                                    Just expectedType ->
                                                        { chunk2UpSeparated
                                                            | checks =
                                                                chunk2UpSeparated.checks
                                                                    |> (::) { checkedExpression = checked, expectation = IsOfType expectedType }
                                                        }
                                                            |> Ok

                                            Nothing ->
                                                -- not a check
                                                { chunk2UpSeparated
                                                    | withoutChecks =
                                                        [ chunk0, "\n\n", chunk1, "\n\n", chunk2UpSeparated.withoutChecks ] |> String.concat
                                                }
                                                    |> Ok
                            )


type CodeSnippetParseError
    = CodeSnippetExpectationParseError String
    | CodeSnippetDeclarationsAndImportsParseError String
