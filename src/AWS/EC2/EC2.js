"use strict"

const EC2 = require('aws-sdk/clients/ec2')

exports.newEC2 = (params) =>
  () => new EC2(params)

exports.describeInstancesImpl = (ec2, filters) =>
  () => {
    if (filters.length == 0) {
      return ec2.describeInstances().promise()
    }
    else {
      return ec2.describeInstances({ Filters: filters }).promise()
    }
  }

exports.describeTagsImpl = (ec2, filters) =>
  () => ec2.describeTags({ Filters: filters }).promise()

exports.describeInstanceAttributeImpl = (ec2, attribute, instanceId) =>
  () => ec2.describeInstanceAttribute({ Attribute: attribute, InstanceId: instanceId }).promise()

exports.describeInstanceTypesImpl = (ec2, instanceTypes) =>
  () => ec2.describeInstanceTypes({ InstanceTypes: instanceTypes }).promise()