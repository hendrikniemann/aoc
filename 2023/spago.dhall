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
  , "node-event-emitter"
  , "node-process"
  , "node-readline"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
