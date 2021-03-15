"use strict"

const S3 = require('aws-sdk/clients/s3')

exports.newS3 = (params) => () => new S3(params)

exports.getObjectImpl = (s3, params) => () => s3
    .getObject(params)
    .promise()

exports.getSignedUrlImpl = (s3, operation, params) => () => s3
    .getSignedUrlPromise(operation, params)


exports.createBucketImpl = (s3, bucket, config) => () => s3
    .createBucket({ Bucket: bucket, CreateBucketConfiguration: config })
    .promise()

exports.putBucketPolicyImpl = (s3, bucket, policy) => () => s3
    .putBucketPolicy({ Bucket: bucket, Policy: policy })
    .promise()
