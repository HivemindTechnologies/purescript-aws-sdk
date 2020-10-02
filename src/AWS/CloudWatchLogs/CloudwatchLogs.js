"use strict"

const { CloudWatchLogs } = require('aws-sdk')

exports.makeDefaultClientImpl = () => new CloudWatchLogs()

exports.makeClientImpl = (params) =>
  () => new CloudWatchLogs(params)

exports.describeLogGroupsImpl = (cw) =>
  () => cw.describeLogGroups().promise()

exports.describeLogStreamsImpl = (cw, groupName) =>
  () => cw.describeLogStreams({ logGroupName: groupName }).promise()
