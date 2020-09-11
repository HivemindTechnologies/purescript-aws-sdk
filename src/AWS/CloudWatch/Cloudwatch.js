"use strict"

var AWS = require('aws-sdk/clients/cloudwatch')

exports.makeClientImpl = (params) =>
  () => new CloudWatch(params)

exports.getMetricStatisticsImpl = (cw, p) =>
  () => cw.getMetricStatistics(p).promise().then(JSON.stringify)
