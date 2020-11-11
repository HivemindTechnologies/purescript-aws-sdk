"use strict"

var AWS = require('aws-sdk')

exports.makeDefaultClientImpl = () => new AWS.Lambda()

exports.makeClientImpl = (params) =>
  () => new AWS.Lambda(params)


exports.invokeFunctionImpl = (lambda, params) =>
  () => lambda.invokeFunction(params).promise()
