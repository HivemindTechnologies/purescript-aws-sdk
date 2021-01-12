"use strict"

var AWS = require('aws-sdk')

exports.newDynamoDbClient = (params) => () => new AWS.DynamoDB(params)

exports.getItemImpl = (db, params) => () => db.getItem(params).promise()

exports.putItemImpl = (db, params) => () => db.putItem(params).promise()

exports.createTableImpl = (db, params) => () => db.createTable(params).promise()
