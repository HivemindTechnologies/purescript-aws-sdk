"use strict"

const CostExplorer = require('aws-sdk/clients/costexplorer')

exports.newCE = (params) =>
  () => new CostExplorer(params)


exports.getCostAndUsageImpl = (ce, params) =>
  () => ce.getCostAndUsage(params).promise()
