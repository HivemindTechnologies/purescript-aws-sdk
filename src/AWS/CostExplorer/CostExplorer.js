"use strict"

var AWS = require('aws-sdk/clients/costexplorer')

exports.makeClientImpl = (params) =>
  () => new CostExplorer(params)


exports.getCostAndUsageImpl = (ce, params) =>
  () => ce.getCostAndUsage(params).promise()
