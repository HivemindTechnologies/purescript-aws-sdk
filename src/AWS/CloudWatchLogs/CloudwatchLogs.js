"use strict"

const { CloudWatchLogs } = require('aws-sdk')


exports.newCloudWatchLogs = (params) =>
  () => new CloudWatchLogs(params)

exports.describeLogGroupsImpl = (cw) =>
  () => cw.describeLogGroups().promise()

exports.describeLogStreamsImpl = (cw, groupName) =>
  () => cw.describeLogStreams({ logGroupName: groupName }).promise()
