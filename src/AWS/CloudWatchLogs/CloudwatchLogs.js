"use strict"

const { CloudWatchLogs } = require('aws-sdk')


exports.newCloudWatchLogs = (params) =>
  () => new CloudWatchLogs(params)

exports.describeLogGroupsImpl = (cw, params) =>
  () => cw.describeLogGroups(params).promise()

exports.describeLogStreamsImpl = (cw, params) =>
  () => cw.describeLogStreams(params).promise()

exports.putRetentionPolicyImpl = (cw, groupName, retention) =>
  () => cw.putRetentionPolicy({ logGroupName: groupName, retentionInDays: retention }).promise()

exports.deleteRetentionPolicyImpl = (cw, groupName) =>
  () => cw.deleteRetentionPolicy({ logGroupName: groupName }).promise()

exports.createExportTaskImpl = (cw, destination, from, groupName, to) => 
  () => cw.createExportTask({destination: destination, from: from, logGroupName: groupName, to: to}).promise()

exports.listTagsLogGroupImpl = (cw, groupName) =>
  () => cw.listTagsLogGroup({logGroupName: groupName}).promise()
