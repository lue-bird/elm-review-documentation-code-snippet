module Imports exposing (Imports, implicit, insertSyntaxImports)

import Elm.Syntax.Exposing
import Elm.Syntax.Import
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node(..))
import FastDict exposing (Dict)
import List.LocalExtra
import Set exposing (Set)


type alias Imports =
    Dict
        Elm.Syntax.ModuleName.ModuleName
        { alias : Maybe String
        , exposed : Set String -- includes names of variants
        }


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicit : Imports
implicit =
    [ ( [ "Basics" ]
      , { alias = Nothing
        , exposed =
            [ "Int"
            , "Float"
            , "+"
            , "-"
            , "*"
            , "/"
            , "//"
            , "^"
            , "toFloat"
            , "round"
            , "floor"
            , "ceiling"
            , "truncate"
            , "=="
            , "/="
            , "<"
            , ">"
            , "<="
            , ">="
            , "max"
            , "min"
            , "compare"
            , "Order"
            , "LT"
            , "EQ"
            , "GT"
            , "Bool"
            , "True"
            , "False"
            , "not"
            , "&&"
            , "||"
            , "xor"
            , "++"
            , "modBy"
            , "remainderBy"
            , "negate"
            , "abs"
            , "clamp"
            , "sqrt"
            , "logBase"
            , "e"
            , "pi"
            , "cos"
            , "sin"
            , "tan"
            , "acos"
            , "asin"
            , "atan"
            , "atan2"
            , "degrees"
            , "radians"
            , "turns"
            , "toPolar"
            , "fromPolar"
            , "isNaN"
            , "isInfinite"
            , "identity"
            , "always"
            , "<|"
            , "|>"
            , "<<"
            , ">>"
            , "Never"
            , "never"
            ]
                |> Set.fromList
        }
      )
    , ( [ "List" ], { alias = Nothing, exposed = Set.fromList [ "List", "(::)" ] } )
    , ( [ "Maybe" ], { alias = Nothing, exposed = Set.fromList [ "Maybe", "Just", "Nothing" ] } )
    , ( [ "Result" ], { alias = Nothing, exposed = Set.fromList [ "Result", "Ok", "Err" ] } )
    , ( [ "String" ], { alias = Nothing, exposed = Set.singleton "String" } )
    , ( [ "Char" ], { alias = Nothing, exposed = Set.singleton "Char" } )
    , ( [ "Tuple" ], { alias = Nothing, exposed = Set.empty } )
    , ( [ "Debug" ], { alias = Nothing, exposed = Set.empty } )
    , ( [ "Platform" ], { alias = Nothing, exposed = Set.singleton "Program" } )
    , ( [ "Platform", "Cmd" ], { alias = Just "Cmd", exposed = Set.singleton "Cmd" } )
    , ( [ "Platform", "Sub" ], { alias = Just "Sub", exposed = Set.singleton "Sub" } )
    ]
        |> FastDict.fromList


{-| Merge a given new import with the existing import lookup.
This is strongly preferred over Dict.insert since the implicit default imports can be overridden
-}
insertSyntaxImports :
    Dict
        Elm.Syntax.ModuleName.ModuleName
        { exposedChoiceTypesExposingVariants : Dict String (Set String)
        , exposedValueAndFunctionAndTypeAliasNames : Set String
        }
    -> List (Node Elm.Syntax.Import.Import)
    -> Imports
    -> Imports
insertSyntaxImports byModule syntaxImports =
    \imports ->
        List.foldl
            (\(Node _ import_) importsSoFar ->
                let
                    moduleName : Elm.Syntax.ModuleName.ModuleName
                    moduleName =
                        import_.moduleName |> Elm.Syntax.Node.value

                    importInfo : { exposed : Set String, alias : Maybe String }
                    importInfo =
                        import_
                            |> importContext
                                (byModule
                                    |> FastDict.get moduleName
                                    |> Maybe.withDefault
                                        { exposedChoiceTypesExposingVariants = FastDict.empty
                                        , exposedValueAndFunctionAndTypeAliasNames = Set.empty
                                        }
                                )
                in
                importsSoFar
                    |> insert moduleName
                        { alias = importInfo.alias, exposed = importInfo.exposed }
            )
            imports
            syntaxImports


importContext :
    { exposedChoiceTypesExposingVariants : Dict String (Set String)
    , exposedValueAndFunctionAndTypeAliasNames : Set String
    }
    -> (Elm.Syntax.Import.Import -> { exposed : Set String, alias : Maybe String })
importContext moduleExposes import_ =
    { alias = import_.moduleAlias |> Maybe.map (\(Node _ parts) -> parts |> String.join ".")
    , exposed =
        case import_.exposingList of
            Nothing ->
                Set.empty

            Just (Node _ existingExposing) ->
                case existingExposing of
                    Elm.Syntax.Exposing.All _ ->
                        Set.union
                            moduleExposes.exposedValueAndFunctionAndTypeAliasNames
                            (moduleExposes |> .exposedChoiceTypesExposingVariants |> FastDict.keys |> Set.fromList)

                    Elm.Syntax.Exposing.Explicit exposes ->
                        exposes
                            |> List.foldl
                                (\(Node _ expose) soFar ->
                                    case expose of
                                        Elm.Syntax.Exposing.FunctionExpose exposeValueReferenceName ->
                                            soFar |> Set.insert exposeValueReferenceName

                                        Elm.Syntax.Exposing.TypeOrAliasExpose typeName ->
                                            soFar |> Set.insert typeName

                                        Elm.Syntax.Exposing.InfixExpose symbol ->
                                            soFar |> Set.insert symbol

                                        Elm.Syntax.Exposing.TypeExpose typeExpose ->
                                            Set.union
                                                soFar
                                                (moduleExposes
                                                    |> .exposedChoiceTypesExposingVariants
                                                    |> FastDict.get typeExpose.name
                                                    |> Maybe.withDefault Set.empty
                                                    |> Set.insert typeExpose.name
                                                )
                                )
                                Set.empty
    }


insert : Elm.Syntax.ModuleName.ModuleName -> { alias : Maybe String, exposed : Set String } -> (Imports -> Imports)
insert moduleName importInfoToAdd imports =
    FastDict.update moduleName
        (\existingImport ->
            let
                newImportInfo : { alias : Maybe String, exposed : Set String }
                newImportInfo =
                    case existingImport of
                        Nothing ->
                            importInfoToAdd

                        Just import_ ->
                            { alias = List.LocalExtra.firstJustMap .alias [ import_, importInfoToAdd ]
                            , exposed = Set.union import_.exposed importInfoToAdd.exposed
                            }
            in
            Just newImportInfo
        )
        imports
