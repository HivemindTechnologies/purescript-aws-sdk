"use strict"

const KMS = require('aws-sdk/clients/kms')

exports.newKMS = (params) => () => new KMS(params)

exports.encryptImpl = (kms, params) => () => kms
    .encrypt(params)
    .promise()

exports.decryptImpl = (kms, params) => () => kms
    .decrypt(params)
    .promise()
