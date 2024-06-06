#### 1.1.1
  - avoid unnecessary parens in expectation to fix elm-review trying to edit code again after elm-format has run (https://github.com/lue-bird/elm-review-documentation-code-snippet/issues/4)

### 1.1.0
  - `check` now requires an explicit import of the local module if you want to expose members
  - the previous behaviour of `check` (implicit import of the current module `exposing (..)`) can now be achieved using `checkImplicitlyImportingEverythingFromCurrentModule`
  - adds an error for unknown references (usually missing imports)
  - improves parsing error messages to be more precise

#### 1.0.1
  - update mentions to the old module name `Review.Documentation.Example`
    to `Review.Documentation.CodeSnippet`
