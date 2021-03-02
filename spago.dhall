{ name = "purescript-aws-sdk-basic"
, dependencies =
  [ "aff-promise"
  , "argonaut"
  , "console"
  , "datetime"
  , "effect"
  , "foreign"
  , "formatters"
  , "js-date"
  , "justifill"
  , "monad-control"
  , "node-buffer"
  , "nullable"
  , "numbers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/HivemindTechnologies/purescript-aws-sdk.git"
}
