"use strict"

var AWS = require('aws-sdk')

exports.newSharedIniFileCredentials = (params) => () => {
    console.log("Params SharedIniFileCredentials: ", params);
    return new AWS.SharedIniFileCredentials(params);}
