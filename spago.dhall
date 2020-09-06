{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "behaviors"
  , "canvas"
  , "console"
  , "debug"
  , "drawing"
  , "effect"
  , "generics-rep"
  , "profunctor-lenses"
  , "psci-support"
  , "record-extra"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
