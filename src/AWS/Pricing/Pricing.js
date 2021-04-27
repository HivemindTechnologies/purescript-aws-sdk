
"use strict"

const Pricing = require('aws-sdk/clients/pricing')

exports.newPricing = (params) =>
    () => new Pricing(params)

exports.getProductsImpl = (client, filters, serviceCode, token, max) =>
    () => client.getProducts({ Filters: filters, ServiceCode: serviceCode, NextToken: token, MaxResults: max }).promise()
