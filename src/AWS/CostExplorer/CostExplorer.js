"use strict"

var AWS = require('aws-sdk')

exports.makeClientImpl = (params) =>
  () => new AWS.CostExplorer(params)


exports.getCostAndUsageImpl = (ce, params) =>
  () => ce.getCostAndUsage(params).promise()
