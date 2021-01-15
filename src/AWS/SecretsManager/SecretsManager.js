"use strict"

const SecretsManager = require('aws-sdk/clients/secretsmanager')

exports.newSecretsManager = (params) => () => new SecretsManager(params)

exports.getSecretValueImpl = (sm, params) => () => sm
    .getSecretValue(params)
    .promise()
