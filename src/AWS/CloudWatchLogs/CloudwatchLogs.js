"use strict"

const CloudWatchLogs = require('aws-sdk/clients/cloudwatchlogs')

exports.makeDefaultClientImpl = () => new CloudWatchLogs()

exports.makeClientImpl = (params) =>
  () => new CloudWatchLogs(params)

exports.describeLogGroupsImpl = (cw) =>
  () => cw.describeLogGroups().promise().then(JSON.stringify)

exports.describeLogStreamsImpl = (cw, groupName) =>
  () => cw.describeLogStreams({ logGroupName: groupName }).promise()
