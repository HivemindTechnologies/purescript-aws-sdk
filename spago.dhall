{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-aws-sdk"
, dependencies =
  [ "aff-promise"
  , "console"
  , "datetime"
  , "effect"
  , "formatters"
  , "js-date"
  , "justifill"
  , "monad-control"
  , "numbers"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
