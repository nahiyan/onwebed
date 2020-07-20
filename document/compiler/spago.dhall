{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "psci-support"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
