module Review.Documentation.CodeSnippet exposing (check, checkImplicitlyImportingEverythingFromCurrentModule)

{-| Checks your small code examples in the readme, module headers and declaration comments
for valid syntax, matching types and correctness
by generating tests from these code snippets.

    import Review.Documentation.CodeSnippet

    config =
        [ Review.Documentation.CodeSnippet.check
        ]

@docs check, checkImplicitlyImportingEverythingFromCurrentModule

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
import Elm.Syntax.Node
import Elm.Syntax.Range exposing (Range)
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
import Review.Rule
import RoughMarkdown
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


type alias ProjectContext =
    { documentationCodeSnippetTestModule : Maybe { key : Review.Rule.ModuleKey, source : String }
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
    , readmeKey : Maybe Review.Rule.ReadmeKey
    , readmeCodeSnippets : List CodeSnippet
    }


type alias CodeSnippet =
    { startRow : Int
    , imports : List Elm.Syntax.Import.Import
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
    | TestModuleContext Review.Rule.ModuleKey


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


toMarked : String -> (String -> Maybe String)
toMarked mark =
    \string ->
        case string |> String.lines |> List.LocalExtra.allJustMap (stringToWithoutStart mark) of
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


elmCodeBlockChunksSplitOffChecks :
    List String
    -> Result CodeSnippetChecksElement { withoutChecks : String, checks : List CodeSnippetCheck }
elmCodeBlockChunksSplitOffChecks =
    \chunks -> chunks |> elmCodeBlockChunksSplitOffChecksFromIndex 0


elmCodeBlockChunksSplitOffChecksFromIndex :
    Int
    ->
        (List String
         -> Result CodeSnippetChecksElement { withoutChecks : String, checks : List CodeSnippetCheck }
        )
elmCodeBlockChunksSplitOffChecksFromIndex nextCheckIndex =
    \chunks ->
        -- IGNORE TCO
        case chunks of
            [] ->
                { withoutChecks = "", checks = [] } |> Ok

            [ onlyChunk ] ->
                { withoutChecks = onlyChunk, checks = [] } |> Ok

            chunk0 :: chunk1 :: chunk2Up ->
                [ \() ->
                    case chunk1 |> toMarked "-->" of
                        Just expectedExpressionSource ->
                            case expectedExpressionSource |> ElmSyntaxParse.expression of
                                Nothing ->
                                    EqualsKind |> Err |> Just

                                Just expectedExpression ->
                                    Equals expectedExpression
                                        |> Ok
                                        |> Just

                        Nothing ->
                            Nothing
                , \() ->
                    case chunk1 |> toMarked "--:" of
                        Just expectedTypeSource ->
                            case expectedTypeSource |> ElmSyntaxParse.type_ of
                                Nothing ->
                                    IsOfTypeKind |> Err |> Just

                                Just expectedType ->
                                    IsOfType expectedType
                                        |> Ok
                                        |> Just

                        Nothing ->
                            Nothing
                ]
                    |> List.LocalExtra.firstJustMap (\f -> f ())
                    |> (\maybe ->
                            case maybe of
                                Just (Err expectationKind) ->
                                    { checkIndex = nextCheckIndex
                                    , part = CodeSnippetExpectation
                                    , expectationKind = expectationKind
                                    }
                                        |> Err

                                Just (Ok expectation) ->
                                    case chunk0 |> ElmSyntaxParse.expression of
                                        Nothing ->
                                            { checkIndex = nextCheckIndex
                                            , part = CodeSnippetCheckedExpression
                                            , expectationKind =
                                                case expectation of
                                                    Equals _ ->
                                                        EqualsKind

                                                    IsOfType _ ->
                                                        IsOfTypeKind
                                            }
                                                |> Err

                                        Just checkedExpression ->
                                            chunk2Up
                                                |> elmCodeBlockChunksSplitOffChecks
                                                |> Result.map
                                                    (\chunk2UpSeparated ->
                                                        { chunk2UpSeparated
                                                            | checks =
                                                                chunk2UpSeparated.checks
                                                                    |> (::) { checkedExpression = checkedExpression, expectation = expectation }
                                                        }
                                                    )

                                -- not a check
                                Nothing ->
                                    (chunk1 :: chunk2Up)
                                        |> elmCodeBlockChunksSplitOffChecks
                                        |> Result.map
                                            (\chunk1UpSeparated ->
                                                { chunk1UpSeparated
                                                    | withoutChecks =
                                                        [ chunk0, "\n\n", chunk1UpSeparated.withoutChecks ] |> String.concat
                                                }
                                            )
                       )


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
                            (\(Elm.Syntax.Node.Node _ expose) ->
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
                            (\(Elm.Syntax.Node.Node _ expose) ->
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


checkWith : ImplicitImportCurrentModuleExposing -> Review.Rule.Rule
checkWith implicitImportCurrentModuleExposing =
    Review.Rule.newProjectRuleSchema "Review.Documentation.CodeSnippet" initialProjectContext
        |> Review.Rule.providesFixesForProjectRule
        |> Review.Rule.withReadmeProjectVisitor
            (\maybeReadme context ->
                case maybeReadme of
                    Nothing ->
                        ( [], context )

                    Just readme ->
                        let
                            codeSnippetsAndErrors : List (Result { startRow : Int, error : LocationInCodeSnippet } CodeSnippet)
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
                                        (error.error |> codeSnippetParseErrorInfo)
                                        { start = { row = error.startRow, column = 0 }, end = { row = error.startRow + 1, column = 0 } }
                                )
                        , { context
                            | readmeKey = readme.readmeKey |> Just
                            , readmeCodeSnippets =
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
                                        ( moduleDocs.name |> String.lines
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
                        (\(Elm.Syntax.Node.Node _ moduleHeader) context ->
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
                                                codeSnippetsAndErrors : List (Result { startRow : Int, error : LocationInCodeSnippet } CodeSnippet)
                                                codeSnippetsAndErrors =
                                                    moduleHeaderDocumentation
                                                        |> markdownElmCodeBlocksInModule
                                                        |> List.filterMap elmCodeBlockToSnippet
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
                                                            (error.error |> codeSnippetParseErrorInfo)
                                                            { start = { row = error.startRow, column = 0 }, end = { row = error.startRow + 1, column = 0 } }
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
                        (\(Elm.Syntax.Node.Node _ declaration) context ->
                            case context of
                                TestModuleContext testModuleContext ->
                                    ( [], testModuleContext |> TestModuleContext )

                                NonTestModuleContext nonTestModuleContext ->
                                    case declaration |> declarationToDocumented of
                                        Nothing ->
                                            ( [], nonTestModuleContext |> NonTestModuleContext )

                                        Just memberDocumented ->
                                            let
                                                codeSnippetsAndErrors : List (Result { startRow : Int, error : LocationInCodeSnippet } CodeSnippet)
                                                codeSnippetsAndErrors =
                                                    memberDocumented.documentation
                                                        |> markdownElmCodeBlocksInModule
                                                        |> List.filterMap elmCodeBlockToSnippet
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
                                                            (error.error |> codeSnippetParseErrorInfo)
                                                            { start = { row = error.startRow, column = 0 }, end = { row = error.startRow + 1, column = 0 } }
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
        |> Review.Rule.withFinalProjectEvaluation
            (\fullContext -> fullContext |> checkFullProject implicitImportCurrentModuleExposing)
        |> Review.Rule.fromProjectRuleSchema


commentToModuleHeader :
    { resources_ | extractSourceCode : Range -> String }
    -> (Elm.Syntax.Node.Node String -> Maybe (Elm.Syntax.Node.Node String))
commentToModuleHeader resources =
    \(Elm.Syntax.Node.Node commentRange comment) ->
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


indexthToString : Int -> String
indexthToString index =
    let
        suffix : String
        suffix =
            case (index + 1) |> remainderBy 10 of
                1 ->
                    "st"

                2 ->
                    "nd"

                3 ->
                    "rd"

                _ ->
                    "th"
    in
    String.fromInt (index + 1) ++ suffix


checkFullProject :
    ImplicitImportCurrentModuleExposing
    -> (ProjectContext -> List (Review.Rule.Error { useErrorForModule : () }))
checkFullProject implicitImportCurrentModuleExposing =
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
                    toError : { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) } -> { info : { message : String, details : List String }, range : Range }
                    toError =
                        \invalid ->
                            let
                                pluralizedForUnknownReferences : { name : String, beNot : String }
                                pluralizedForUnknownReferences =
                                    case invalid.unknownReferences |> Set.size of
                                        1 ->
                                            { name = "name", beNot = "isn't" }

                                        _ ->
                                            { name = "names", beNot = "aren't" }
                            in
                            { info =
                                { message = "documentation code snippet uses unknown references"
                                , details =
                                    [ [ case invalid.location of
                                            CodeSnippetDeclarationsAndImports ->
                                                "The declarations use"

                                            CodeSnippetChecksElement codeSnippetChecksElement ->
                                                let
                                                    expectationInfo : { marker : String, nextDescription : String }
                                                    expectationInfo =
                                                        case codeSnippetChecksElement.expectationKind of
                                                            EqualsKind ->
                                                                { marker = "-->", nextDescription = "expression" }

                                                            IsOfTypeKind ->
                                                                { marker = "--:", nextDescription = "type" }
                                                in
                                                case codeSnippetChecksElement.part of
                                                    CodeSnippetExpectation ->
                                                        [ "The expected "
                                                        , expectationInfo.nextDescription
                                                        , " in the "
                                                        , codeSnippetChecksElement.checkIndex |> indexthToString
                                                        , " check (after the "
                                                        , expectationInfo.marker
                                                        , " marker) uses"
                                                        ]
                                                            |> String.concat

                                                    CodeSnippetCheckedExpression ->
                                                        [ "The checked expression in the "
                                                        , codeSnippetChecksElement.checkIndex |> indexthToString
                                                        , " check (just before the "
                                                        , expectationInfo.marker
                                                        , " marker) uses"
                                                        ]
                                                            |> String.concat
                                      , " the "
                                      , pluralizedForUnknownReferences.name
                                      , " "
                                      , invalid.unknownReferences |> Set.toList |> List.map referenceToString |> String.join " and "
                                      , " which "
                                      , pluralizedForUnknownReferences.beNot
                                      , " imported or defined there."
                                      ]
                                        |> String.concat
                                    , "Maybe they there's a typo or missing import? Known that no members of any of your modules are exposed by default. To implicitly expose all members from the current module, use checkImplicitlyImportingEverythingFromCurrentModule: https://dark.elm.dmy.fr/packages/lue-bird/elm-documentation-code-snippet/latest/Review-Documentation-CodeSnippet#checkImplicitlyImportingEverythingFromCurrentModule"
                                    ]
                                }
                            , range = { start = { row = invalid.startRow, column = 0 }, end = { row = invalid.startRow + 1, column = 0 } }
                            }

                    testFileAndErrors : { testFile : Elm.CodeGen.File, invalidInModules : List { key : Review.Rule.ModuleKey, errors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) } }, readmeErrors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) } }
                    testFileAndErrors =
                        { readmeCodeSnippets = context.readmeCodeSnippets
                        , exposesByModule = context.exposesByModule
                        , codeSnippetsByModule =
                            context.codeSnippetsByModule
                                |> FastDict.map
                                    (\moduleName moduleCodeSnippets ->
                                        let
                                            addImplicitImport : CodeSnippet -> CodeSnippet
                                            addImplicitImport =
                                                \codeSnippet ->
                                                    { codeSnippet
                                                        | imports =
                                                            codeSnippet.imports
                                                                |> (::)
                                                                    (Elm.CodeGen.importStmt moduleName
                                                                        Nothing
                                                                        (implicitImportCurrentModuleExposing |> implicitImportCurrentModuleExposingToElm)
                                                                    )
                                                    }
                                        in
                                        { key = moduleCodeSnippets.key
                                        , inModuleHeader =
                                            moduleCodeSnippets.inModuleHeader |> List.map addImplicitImport
                                        , inMembers =
                                            moduleCodeSnippets.inMembers
                                                |> FastDict.map
                                                    (\_ memberCodeSnippets ->
                                                        memberCodeSnippets |> List.map addImplicitImport
                                                    )
                                        }
                                    )
                        }
                            |> documentationCodeSnippetsTestFileAndErrors

                    errors : List (Review.Rule.Error scope_)
                    errors =
                        (case context.readmeKey of
                            Nothing ->
                                []

                            Just readmeKey ->
                                testFileAndErrors.readmeErrors
                                    |> List.map
                                        (\invalid ->
                                            let
                                                errorInfo : { info : { message : String, details : List String }, range : Range }
                                                errorInfo =
                                                    invalid |> toError
                                            in
                                            Review.Rule.errorForReadme readmeKey
                                                errorInfo.info
                                                errorInfo.range
                                        )
                        )
                            ++ (testFileAndErrors.invalidInModules
                                    |> List.concatMap
                                        (\invalidInModule ->
                                            invalidInModule.errors
                                                |> List.map
                                                    (\invalid ->
                                                        let
                                                            errorInfo : { info : { message : String, details : List String }, range : Range }
                                                            errorInfo =
                                                                invalid |> toError
                                                        in
                                                        Review.Rule.errorForModule invalidInModule.key
                                                            errorInfo.info
                                                            errorInfo.range
                                                    )
                                        )
                               )

                    generatedTestFile : String
                    generatedTestFile =
                        testFileAndErrors.testFile |> Elm.Pretty.pretty 80

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
                    Review.Rule.errorForModuleWithFix documentationCodeSnippetTestModule.key
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
                        :: errors

                else
                    errors


referenceToString : ( Elm.Syntax.ModuleName.ModuleName, String ) -> String
referenceToString =
    \( qualification, name ) ->
        case qualification of
            [] ->
                name

            qualificationPart0 :: qualificationPart1Up ->
                [ qualificationPart0 :: qualificationPart1Up |> String.join "."
                , "."
                , name
                ]
                    |> String.concat


documentationCodeSnippetsTestFileAndErrors :
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
    ->
        { testFile : Elm.CodeGen.File
        , invalidInModules :
            List
                { key : Review.Rule.ModuleKey
                , errors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) }
                }
        , readmeErrors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) }
        }
documentationCodeSnippetsTestFileAndErrors =
    \infoRaw ->
        let
            codeSnippetUnknownReferences : CodeSnippet -> List { location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) }
            codeSnippetUnknownReferences =
                \codeSnippet ->
                    let
                        imports : Imports
                        imports =
                            Imports.implicit
                                |> Imports.insertSyntaxImports infoRaw.exposesByModule codeSnippet.imports

                        snippetLocalDeclarationNames : Set String
                        snippetLocalDeclarationNames =
                            codeSnippet.declarations
                                |> Set.LocalExtra.unionFromListMap Declaration.LocalExtra.names

                        referencesToUnknowns : Set ( Elm.Syntax.ModuleName.ModuleName, String ) -> Maybe (Set ( Elm.Syntax.ModuleName.ModuleName, String ))
                        referencesToUnknowns =
                            \allReferences ->
                                let
                                    unuseds : Set ( Elm.Syntax.ModuleName.ModuleName, String )
                                    unuseds =
                                        allReferences
                                            |> Set.LocalExtra.justsMap
                                                (\( qualification, unqualified ) ->
                                                    case ( qualification, unqualified ) |> Origin.determine imports of
                                                        Just _ ->
                                                            Nothing

                                                        Nothing ->
                                                            if snippetLocalDeclarationNames |> Set.member unqualified then
                                                                Nothing

                                                            else
                                                                ( qualification, unqualified ) |> Just
                                                )
                                in
                                if unuseds |> Set.isEmpty then
                                    Nothing

                                else
                                    unuseds |> Just
                    in
                    List.LocalExtra.consJust
                        (codeSnippet.declarations
                            |> Set.LocalExtra.unionFromListMap Declaration.LocalExtra.references
                            |> referencesToUnknowns
                            |> Maybe.map (\unknown -> { unknownReferences = unknown, location = CodeSnippetDeclarationsAndImports })
                        )
                        (codeSnippet.checks
                            |> List.indexedMap
                                (\checkIndex codeSnippetCheck ->
                                    case codeSnippetCheck.expectation of
                                        Equals expectedExpression ->
                                            [ (codeSnippetCheck.checkedExpression |> Expression.LocalExtra.references |> referencesToUnknowns)
                                                |> Maybe.map
                                                    (\unknown ->
                                                        { unknownReferences = unknown
                                                        , location = CodeSnippetChecksElement { checkIndex = checkIndex, part = CodeSnippetCheckedExpression, expectationKind = EqualsKind }
                                                        }
                                                    )
                                            , (expectedExpression |> Expression.LocalExtra.references |> referencesToUnknowns)
                                                |> Maybe.map
                                                    (\unknown ->
                                                        { unknownReferences = unknown
                                                        , location = CodeSnippetChecksElement { checkIndex = checkIndex, part = CodeSnippetExpectation, expectationKind = EqualsKind }
                                                        }
                                                    )
                                            ]
                                                |> List.filterMap identity

                                        IsOfType expectedType ->
                                            [ (codeSnippetCheck.checkedExpression |> Expression.LocalExtra.references |> referencesToUnknowns)
                                                |> Maybe.map
                                                    (\unknown ->
                                                        { unknownReferences = unknown
                                                        , location = CodeSnippetChecksElement { checkIndex = checkIndex, part = CodeSnippetCheckedExpression, expectationKind = IsOfTypeKind }
                                                        }
                                                    )
                                            , (expectedType |> Type.LocalExtra.references |> referencesToUnknowns)
                                                |> Maybe.map
                                                    (\unknown ->
                                                        { unknownReferences = unknown
                                                        , location = CodeSnippetChecksElement { checkIndex = checkIndex, part = CodeSnippetExpectation, expectationKind = IsOfTypeKind }
                                                        }
                                                    )
                                            ]
                                                |> List.filterMap identity
                                )
                            |> List.concat
                        )

            codeSnippetsToValidAndErrors :
                List CodeSnippet
                ->
                    { valid : List CodeSnippet
                    , errors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) }
                    }
            codeSnippetsToValidAndErrors =
                \codeSnippets ->
                    codeSnippets
                        |> List.foldr
                            (\codeSnippet soFar ->
                                let
                                    unknowns : List { location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) }
                                    unknowns =
                                        codeSnippet |> codeSnippetUnknownReferences
                                in
                                case unknowns of
                                    [] ->
                                        { soFar | valid = soFar.valid |> (::) codeSnippet }

                                    location0 :: location1Up ->
                                        { soFar
                                            | errors =
                                                ((location0 :: location1Up)
                                                    |> List.map (\l -> { startRow = codeSnippet.startRow, location = l.location, unknownReferences = l.unknownReferences })
                                                )
                                                    ++ soFar.errors
                                        }
                            )
                            { errors = []
                            , valid = []
                            }

            readmeValidAndErrors : { valid : List CodeSnippet, errors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) } }
            readmeValidAndErrors =
                infoRaw.readmeCodeSnippets |> codeSnippetsToValidAndErrors

            modulesValidAndErrors : Dict Elm.Syntax.ModuleName.ModuleName { key : Review.Rule.ModuleKey, inModuleHeader : List CodeSnippet, inMembers : Dict String (List CodeSnippet), errors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) } }
            modulesValidAndErrors =
                infoRaw.codeSnippetsByModule
                    |> FastDict.map
                        (\_ moduleCodeSnippets ->
                            let
                                inModuleHeaderValidAndErrors : { valid : List CodeSnippet, errors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) } }
                                inModuleHeaderValidAndErrors =
                                    moduleCodeSnippets.inModuleHeader |> codeSnippetsToValidAndErrors

                                inMembersValidAndErrors : Dict String { valid : List CodeSnippet, errors : List { startRow : Int, location : LocationInCodeSnippet, unknownReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String ) } }
                                inMembersValidAndErrors =
                                    moduleCodeSnippets.inMembers |> FastDict.map (\_ codeSnippets -> codeSnippets |> codeSnippetsToValidAndErrors)
                            in
                            { key = moduleCodeSnippets.key
                            , inModuleHeader = inModuleHeaderValidAndErrors.valid
                            , inMembers = inMembersValidAndErrors |> FastDict.map (\_ -> .valid)
                            , errors =
                                (inMembersValidAndErrors |> FastDict.values |> List.concatMap (\validAndErr -> validAndErr.errors))
                                    ++ inModuleHeaderValidAndErrors.errors
                            }
                        )
        in
        { testFile =
            { readmeCodeSnippets = readmeValidAndErrors.valid
            , exposesByModule = infoRaw.exposesByModule
            , codeSnippetsByModule =
                modulesValidAndErrors
                    |> FastDict.map
                        (\_ moduleValidAndErrors ->
                            { key = moduleValidAndErrors.key
                            , inModuleHeader = moduleValidAndErrors.inModuleHeader
                            , inMembers = moduleValidAndErrors.inMembers
                            }
                        )
            }
                |> createDocumentationCodeSnippetsTestFile
        , invalidInModules =
            modulesValidAndErrors
                |> FastDict.values
                |> List.map (\v -> { key = v.key, errors = v.errors })
        , readmeErrors = readmeValidAndErrors.errors
        }


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
                                |> Set.LocalExtra.unionFromListMap Declaration.LocalExtra.names

                        referenceFullyQualifyAndAdaptLocationSuffix : ( Elm.Syntax.ModuleName.ModuleName, String ) -> ( Elm.Syntax.ModuleName.ModuleName, String )
                        referenceFullyQualifyAndAdaptLocationSuffix =
                            \( qualification, unqualifiedName ) ->
                                case ( qualification, unqualifiedName ) |> Origin.determine imports of
                                    Nothing ->
                                        if snippetLocalDeclarationNames |> Set.member unqualifiedName then
                                            ( [], [ unqualifiedName, "__", locationSuffix ] |> String.concat )

                                        else
                                            -- should already be reported by as unknown references
                                            ( [], unqualifiedName )

                                    Just moduleName ->
                                        ( moduleName, unqualifiedName )

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
                    { startRow = codeSnippet.startRow
                    , imports = codeSnippet.imports
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

            codeSnippetsFullyQualifyAndAddLocationSuffix :
                String
                -> (List CodeSnippet -> List CodeSnippet)
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
                            in
                            { key = moduleInfoRaw.key
                            , inModuleHeader =
                                moduleInfoRaw.inModuleHeader
                                    |> codeSnippetsFullyQualifyAndAddLocationSuffix (moduleNameLocationSuffix "Header")
                            , inMembers =
                                moduleInfoRaw.inMembers
                                    |> FastDict.map
                                        (\memberName memberCodeSnippets ->
                                            memberCodeSnippets
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

            codeSnippets : List CodeSnippet
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
                        (codeSnippet.declarations |> Set.LocalExtra.unionFromListMap Declaration.LocalExtra.usedModules)
                        (codeSnippet.checks |> Set.LocalExtra.unionFromListMap codeSnippetCheckUsedModules)

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
                    |> Set.LocalExtra.unionFromListMap codeSnippetUsedModules
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
                Elm.CodeGen.pipe codeSnippetCheck.checkedExpression
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


implicitImportCurrentModuleExposingToElm : ImplicitImportCurrentModuleExposing -> Maybe Elm.CodeGen.Exposing
implicitImportCurrentModuleExposingToElm =
    \implicitImportCurrentModuleExposing ->
        case implicitImportCurrentModuleExposing of
            ImplicitImportCurrentModuleExposingNone ->
                Nothing

            ImplicitImportCurrentModuleExposingAll ->
                Elm.CodeGen.exposeAll |> Just


declarationListVisitor : List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration) -> (NonTestModuleContext -> NonTestModuleContext)
declarationListVisitor declarations =
    \context ->
        case context.exposingKind of
            ExposingExplicit ->
                { context
                    | exposedChoiceTypesExposingVariants =
                        declarations
                            |> List.filterMap
                                (\(Elm.Syntax.Node.Node _ declaration) ->
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
                                                        |> List.map (\(Elm.Syntax.Node.Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
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
                                (\(Elm.Syntax.Node.Node _ declaration) ->
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
                                                        |> List.map (\(Elm.Syntax.Node.Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
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
                                (\(Elm.Syntax.Node.Node _ declaration) ->
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


markdownElmCodeBlocksInModule : Elm.Syntax.Node.Node String -> List { startRow : Int, body : String }
markdownElmCodeBlocksInModule =
    \(Elm.Syntax.Node.Node documentationRange documentationString) ->
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
                                    soFar |> (::) { startRow = documentationRange.start.row + codeBlock.startRow - 1, body = codeBlock.body }

                                Just "elm" ->
                                    soFar |> (::) { startRow = documentationRange.start.row + codeBlock.startRow - 1, body = codeBlock.body }

                                Just "" ->
                                    soFar |> (::) { startRow = documentationRange.start.row + codeBlock.startRow - 1, body = codeBlock.body }

                                Just _ ->
                                    soFar
                )
                []
            |> List.reverse


markdownElmCodeBlocksInReadme : String -> List { startRow : Int, body : String }
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
                                    soFar |> (::) { startRow = codeBlock.startRow, body = codeBlock.body }

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
    , readmeKey = Nothing
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
    , readmeKey =
        case a.readmeKey of
            Just readmeKey ->
                readmeKey |> Just

            Nothing ->
                b.readmeKey
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
                    , readmeKey = Nothing
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
                    , readmeKey = Nothing
                    , readmeCodeSnippets = []
                    }
        )
        |> Review.Rule.withModuleKey
        |> Review.Rule.withModuleName
        |> Review.Rule.withSourceCodeExtractor


declarationToDocumented : Declaration -> Maybe { name : String, documentation : Elm.Syntax.Node.Node String }
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


codeSnippetParseErrorInfo : LocationInCodeSnippet -> { message : String, details : List String }
codeSnippetParseErrorInfo =
    \codeSnippetParseError ->
        case codeSnippetParseError of
            CodeSnippetDeclarationsAndImports ->
                { message = "code snippet parsing failed"
                , details =
                    [ "I expected to find syntactically valid elm code here but something is off with the imports/declarations."
                    , "If you don't see an obvious mistake, try moving the code to an elm module and see where the compiler complains."
                    ]
                }

            CodeSnippetChecksElement checkParseError ->
                let
                    expectationInfo : { marker : String, nextDescription : String }
                    expectationInfo =
                        case checkParseError.expectationKind of
                            EqualsKind ->
                                { marker = "-->", nextDescription = "expression" }

                            IsOfTypeKind ->
                                { marker = "--:", nextDescription = "type" }
                in
                case checkParseError.part of
                    CodeSnippetExpectation ->
                        { message = "code snippet expectation parsing failed"
                        , details =
                            [ [ "Whenever I see the expectation marker "
                              , expectationInfo.marker
                              , ", I expect a syntactically valid "
                              , expectationInfo.nextDescription
                              , " next. However, I wasn't able to parse what comes after the "
                              , checkParseError.checkIndex |> indexthToString
                              , " marker among all checks."
                              ]
                                |> String.concat
                            , "If you don't see an obvious mistake, try moving the code to an elm module and see where the compiler complains."
                            ]
                        }

                    CodeSnippetCheckedExpression ->
                        { message = "code snippet expected expression parsing failed"
                        , details =
                            [ [ "Whenever I see the expectation marker "
                              , expectationInfo.marker
                              , ", I expect a syntactically valid expression just before it. However, I wasn't able to parse what comes before the "
                              , checkParseError.checkIndex |> indexthToString
                              , " marker among all checks."
                              ]
                                |> String.concat
                            , "If you don't see an obvious mistake, try moving the code to an elm module and see where the compiler complains."
                            ]
                        }


elmCodeBlockToSnippet : { startRow : Int, body : String } -> Maybe (Result { startRow : Int, error : LocationInCodeSnippet } CodeSnippet)
elmCodeBlockToSnippet =
    \elmCode ->
        case elmCode.body |> elmCodeBlockSplitOffChecks of
            Err toFind ->
                { startRow = elmCode.startRow, error = CodeSnippetChecksElement toFind } |> Err |> Just

            Ok split ->
                case split.checks of
                    [] ->
                        Nothing

                    check0 :: check1Up ->
                        case split.withoutChecks |> ElmSyntaxParse.importsAndDeclarations of
                            Nothing ->
                                { startRow = elmCode.startRow, error = CodeSnippetDeclarationsAndImports }
                                    |> Err
                                    |> Just

                            Just importsAndDeclarations ->
                                { startRow = elmCode.startRow
                                , imports = importsAndDeclarations.imports
                                , declarations = importsAndDeclarations.declarations
                                , checks = check0 :: check1Up
                                }
                                    |> Ok
                                    |> Just


elmCodeBlockSplitOffChecks :
    String
    -> Result CodeSnippetChecksElement { withoutChecks : String, checks : List CodeSnippetCheck }
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
                                    , marked |> String.lines |> List.map (\line -> mark ++ line) |> String.join "\n"
                                    ]

                                -- multiple crammed together without blank lines between
                                Nothing ->
                                    case chunk0AfterReal0 |> String.lines of
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
            |> String.lines
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


{-| A version of [`check`](#check)
primarily intended to offer compatibility with [`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples)
for legacy codebases.

It will implicitly interpret

    module CurrentModule exposing (doSomething)

    {-| Does something

        doSomething --> "something was done"

    -}
    doSomething

as

    module CurrentModule exposing (doSomething)

    {-| Does something

        import CurrentModule exposing (..)

        doSomething --> "something was done"

    -}
    doSomething

History: This was the previous behaviour of the rule in version 1.
[Shadowing issues](https://github.com/lue-bird/elm-review-documentation-code-snippet/issues/2)
initiated the search for better alternatives.
Requiring explicit imports for exposing members of the current module turned out to
make the snippets easier to understand, easier to copy into your code
and less error prone.
See above linked issue to see considered alternatives or suggest one yourself.

-}
checkImplicitlyImportingEverythingFromCurrentModule : Review.Rule.Rule
checkImplicitlyImportingEverythingFromCurrentModule =
    checkWith ImplicitImportCurrentModuleExposingAll


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
            |> Dict.Extra.keySet
        --> Set.fromList [ 0, 1, 2 ]

        -- or
        Dict.fromList [ ( 0, A ), ( 1, B ), ( 2, C ) ]
            |> Dict.Extra.keySet
        -->
        Set.fromList
            [ 0
            , 1
            , 2
            ]

        -- or
        Dict.fromList [ ( 0, A ), ( 1, B ), ( 2, C ) ]
            |> Dict.Extra.keySet
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

        import Codec exposing (Codec)

        type alias Point =
            { x : Int, y : Int }

        Codec.record (\x y -> { x = x, y = y })
            |> Codec.field .x Codec.signedInt
            |> Codec.field .y Codec.signedInt
            |> Codec.recordFinish
        --: Codec Point
    -}
    record = ...

Note that because you use the type `Codec` _unqualified_ in your check,
you have to _explicitly import_ it.
(This is different from [`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples)
which implicitly exposes all local module members.
In case you want to simulate that behaviour for legacy reasons, use [`checkImplicitlyImportingEverythingFromCurrentModule`](#checkImplicitlyImportingEverythingFromCurrentModule))

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
check : Review.Rule.Rule
check =
    checkWith ImplicitImportCurrentModuleExposingNone


type ImplicitImportCurrentModuleExposing
    = ImplicitImportCurrentModuleExposingNone
    | ImplicitImportCurrentModuleExposingAll


type LocationInCodeSnippet
    = CodeSnippetChecksElement CodeSnippetChecksElement
    | CodeSnippetDeclarationsAndImports


type alias CodeSnippetChecksElement =
    RecordWithoutConstructorFunction
        { checkIndex : Int, part : CodeSnippetCheckPart, expectationKind : CodeSnippetExpectationKind }


type CodeSnippetCheckPart
    = CodeSnippetExpectation
    | CodeSnippetCheckedExpression


type CodeSnippetExpectationKind
    = EqualsKind
    | IsOfTypeKind
