{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "bigints"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "matrices"
  , "maybe"
  , "node-process"
  , "node-readline"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
