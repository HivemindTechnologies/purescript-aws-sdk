
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0/packages.dhall sha256:710b53c085a18aa1263474659daa0ae15b7a4f453158c4f60ab448a6b3ed494e

let overrides = {=}

let additions = {
    spec =
          { dependencies =
            [ "avar"
            , "console"
            , "aff"
            , "exceptions"
            , "strings"
            , "prelude"
            , "transformers"
            , "foldable-traversable"
            , "pipes"
            , "ansi"
            , "fork"
            , "now"
            ]
          , repo = "https://github.com/purescript-spec/purescript-spec.git"
          , version = "v5.0.0"
          }
    , justifill = {
        dependencies = ["record", "spec", "typelevel-prelude" ]
          , repo = "https://github.com/i-am-the-slime/purescript-justifill"
          , version = "0a4d33b"
    }
}

in  upstream // overrides // additions
