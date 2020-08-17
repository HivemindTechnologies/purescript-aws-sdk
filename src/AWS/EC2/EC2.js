"use strict"

var AWS = require('aws-sdk')

exports.makeClientImpl = (params) =>
  () => new AWS.EC2(params)

exports.describeInstancesImpl = (ec2) =>
  () => ec2.describeInstances().promise()
