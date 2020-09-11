"use strict"

var AWS = require('aws-sdk')

exports.makeDefaultClientImpl = () => new AWS.CloudWatchLogs()

exports.makeClientImpl = (params) =>
  () => new AWS.CloudWatchLogs(params)

exports.describeLogGroupsImpl = (cw) =>
  () => cw.describeLogGroups().promise().then(JSON.stringify)
