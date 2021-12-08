{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "node-process"
  , "node-readline"
  , "ordered-collections"
  , "psci-support"
  , "string-parsers"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
