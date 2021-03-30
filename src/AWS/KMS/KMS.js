"use strict"

const KMS = require('aws-sdk/clients/kms')

exports.newKMS = (params) => () => new KMS(params)

exports.encryptImpl = (kms, params) => () => kms
    .encrypt(params)
    .promise()

exports.decryptImpl = (kms, params) => () => {
    console.log("kms call params", params)
    const p = kms
    .decrypt(params)
    .promise()

    p.then(console.log)
    return p
}
