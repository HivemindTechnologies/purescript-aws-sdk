"use strict"

const STS = require('aws-sdk/clients/sts')

// https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/STS.html
exports.newSTS = (params) => () => new STS(params)

exports.assumeRoleImpl = (sts, params) =>
  () => sts.assumeRole(params).promise()

