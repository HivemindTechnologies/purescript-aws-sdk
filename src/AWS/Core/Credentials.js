"use strict"

var AWS = require('aws-sdk')

exports.newSharedIniFileCredentials = (params) => () => new AWS.SharedIniFileCredentials(params)
