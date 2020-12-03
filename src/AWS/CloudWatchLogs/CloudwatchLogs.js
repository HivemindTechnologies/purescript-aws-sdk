"use strict"

const { CloudWatchLogs } = require('aws-sdk')


exports.newCloudWatchLogs = (params) =>
  () => new CloudWatchLogs(params)

exports.describeLogGroupsImpl = (cw) =>
  () => cw.describeLogGroups().promise()

exports.describeLogStreamsImpl = (cw, groupName) =>
  () => cw.describeLogStreams({ logGroupName: groupName }).promise()

exports.putRetentionPolicyImpl = (cw, groupName, retention) =>
  () => cw.putRetentionPolicy({ logGroupName: groupName, retentionInDays: retention }).promise()
