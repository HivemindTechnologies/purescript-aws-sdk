"use strict"

var AWS = require('aws-sdk')

exports.newDynamoDbClient = (params) => () => {
    console.log("Params newDynamoDbClient: ", params);
    return new AWS.DynamoDB(params);}

exports.getItemImpl = (db, params) => () => db.getItem(params).promise()

exports.putItemImpl = (db, params) => () => {
    console.log("Params putItemImpl: ", params);
    console.log("Db putItemImpl: ", db);
    return db.putItem(params).promise();}

exports.createTableImpl = (db, params) => () => db.createTable(params).promise()
