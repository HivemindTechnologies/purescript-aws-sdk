"use strict"

var AWS = require('aws-sdk')

exports.makeClientImpl = (params) =>
  () => new AWS.CloudWatch(params)

exports.getMetricStatisticsImpl = (cw, p) =>
  () => cw.getMetricStatistics(p).promise().then(JSON.stringify)
