{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "onwebed"
, dependencies = [ "console", "effect", "optparse", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
