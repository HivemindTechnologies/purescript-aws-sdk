"use strict"

const STS = require('aws-sdk/clients/sts')

exports.makeDefaultClientImpl = () => new STS()

// https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/STS.html
exports.makeClientImpl = (params) =>
  () => new STS(params)

exports.makeClientImpl2 = (params) =>
  () => new STS(params)


exports.assumeRoleImpl = (sts, params) =>
  () => sts.assumeRole(params).promise()
