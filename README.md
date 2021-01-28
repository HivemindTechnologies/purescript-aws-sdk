# purescript-aws-sdk

Purescript wrapper for AWS SDK. See [usage](#usage)

## tl;dr

This is a Purescript wrapper for the AWS JavaScript SDK that we use for our projects at [Hivemind Technologies AG](https://hivemindtechnologies.com). Rather than aiming for a full compatibility of the AWS SDK, we strive for a wrapper that is Purescript-idomatic and covers the things that we need. If you are looking for a more complete but less idiomatic library, you might want to have a look into [purescript-aws-sdk](https://github.com/purescript-aws-sdk), which auto-generates Purescript code from the AWS SDK sources.

Currently we have some functionality for the following modules:

* CloudWatch
* CloudWatchLogs
* CostExplorer
* EC2
* Lambda
* SecurityTokenService (STS)
* DynamoDB
* SecretsManager
* KMS
* S3

While we do not have any plans to support other features that we currently don't use, we do welcome contributions of missing features.

## Installation

Add `purescript-aws-sdk` to your `packages.dhall` and `spago.dhall`. For more information on how to add a dependency to your spago project, see the [spago documentation](https://github.com/purescript/spago#add-a-package-to-the-package-set).

❗Don't forget to update the **version** ot the repo according to the available [tags](https://github.com/HivemindTechnologies/purescript-aws-sdk/tags).

## Usage

```purescript
import Prelude
import AWS.Lambda as AWSLambda
import AWS.Core.Types
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

mydata = { name: "" } -- create some data 

type Result = { hello :: String } -- define some result type
  
callLambda :: Aff Result
callLambda = do
  client <- liftEffect $ AWSLambda.makeClient {} -- create the client 
  let
    arn = Arn "arn:aws:lambda:<<REGION>>:<<ACCOUNT>>:function:<<LAMBDA-NAME>>" -- set the arn of your lambda
  result <- AWSLambda.invoke client arn mydata  -- invoke the lambda
  pure result
```

The other modules work in a similar way. You create the client first, and then call a function passing the client as well as other parameters.

You can customise the client by passing in any additonal options, e.g.:

```purescript
EC2.makeClient
  { region: Region "eu-west-1"
  , accessKeyId : AccessKeyId "my-access-key-id"
  , secretAccessKey : AccessKeyId "my-secret-access-key"
  , sessionToken : SessionToken "my-session-token"
  }
```
