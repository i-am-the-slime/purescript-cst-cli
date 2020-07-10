{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "either"
    , "generics-rep"
    , "maybe"
    , "profunctor-lenses"
    , "psci-support"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
