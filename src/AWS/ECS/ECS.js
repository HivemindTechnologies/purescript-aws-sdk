"use strict"

const ECS = require('aws-sdk/clients/ecs')

exports.newECS = (params) =>
  () => new ECS(params)

exports.listClustersImpl = (ecs) =>
  () => ecs.listClusters().promise()
