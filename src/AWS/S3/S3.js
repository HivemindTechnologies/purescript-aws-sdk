"use strict"

const S3 = require('aws-sdk/clients/s3')

exports.newS3 = (params) => () => new S3(params)

exports.getObjectImpl = (s3, params) => () => s3
    .getObject(params)
    .promise()

exports.createBucketImpl = (s3, bucket, config) =>
      () => s3.createBucket({ Bucket: bucket, CreateBucketConfiguration: config }).promise()
