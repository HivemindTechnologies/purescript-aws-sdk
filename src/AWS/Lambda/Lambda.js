"use strict"

var AWS = require('aws-sdk')

exports.newLambda = (params) =>
  () => new AWS.Lambda(params)

exports.invokeImpl = (lambda, params) =>
  () => lambda.invoke(params).promise()
