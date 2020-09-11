"use strict"

var AWS = require('aws-sdk')

exports.makeClientImpl = (params) =>
  () => new AWS.CloudWatchLogs(params)

exports.describeLogGroupsImpl = (cw) =>
  () => cw.describeLogGroups()
