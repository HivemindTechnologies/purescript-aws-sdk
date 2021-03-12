"use strict"

const EC2 = require('aws-sdk/clients/ec2')

exports.newEC2 = (params) =>
  () => new EC2(params)

exports.describeInstancesImpl = (ec2) =>
  () => ec2.describeInstances().promise()

exports.describeTagsImpl = (ec2, filters) =>
  () => ec2.describeTags({ Filters: filters }).promise()
