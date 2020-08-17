"use strict"

var AWS = require('aws-sdk')

exports.makeDefaultClientImpl = () => new AWS.STS()

// https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/STS.html
exports.makeClientImpl = (params) =>
  () => new AWS.STS(params)


exports.assumeRoleImpl = (sts, params) =>
  () => sts.assumeRole(params).promise()
