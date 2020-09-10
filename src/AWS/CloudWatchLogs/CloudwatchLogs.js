"use strict"

var AWS = require('aws-sdk')

exports.makeClientImpl = (params) =>
  () => new AWS.CloudWatchLogs(params)

exports.getDescribeLogGroupsImpl = (cw) =>
  () => cw.describeLogGroups()
