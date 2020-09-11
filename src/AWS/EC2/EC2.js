"use strict"

var AWS = require('aws-sdk/clients/ec2')

exports.makeClientImpl = (params) =>
  () => new EC2(params)

exports.describeInstancesImpl = (ec2) =>
  () => ec2.describeInstances().promise()
