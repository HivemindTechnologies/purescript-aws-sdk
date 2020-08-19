# purescript-aws-sdk

Purescript wrapper for the AWS-sdk.

## How to use it

In your `packages.dhall` add the following:

```
let additions =
  { purescript-aws-sdk =
       { dependencies = 
            [ "aff-promise"
            , "console"
            , "datetime"
            , "effect"
            , "formatters"
            , "js-date"
            , "monad-control"
            , "numbers"
            , "simple-json"
            ]
       , repo =
           "https://github.com/HivemindTechnologies/purescript-aws-sdk.git"
       , version =
           "v0.1.0"
       }
  }
```

❗Don't forget to update the **version** ot the repo according to the available [tags](https://github.com/HivemindTechnologies/purescript-aws-sdk/tags).


In your `spago.dhall` also add `"purescript-aws-sdk"` into the list of your dependencies.
