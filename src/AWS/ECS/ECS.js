"use strict"

const ECS = require('aws-sdk/clients/ecs')

exports.newECS = (params) =>
  () => new ECS(params)

exports.listClustersImpl = (ecs) =>
  () => ecs.listClusters().promise()

exports.listTasksImpl = (ecs, clusterName) =>
  () => ecs.listTasks({ cluster: clusterName }).promise()
