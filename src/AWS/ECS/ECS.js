"use strict"

const ECS = require('aws-sdk/clients/ecs')

exports.newECS = (params) =>
  () => new ECS(params)

exports.listClustersImpl = (ecs) =>
  () => ecs.listClusters().promise()

exports.listTasksImpl = (ecs, clusterArn, containerInstanceArn) =>
  () => ecs.listTasks({ cluster: clusterArn, containerInstance: containerInstanceArn }).promise()

exports.listContainerInstancesImpl = (ecs, clusterArn) =>
  () => ecs.listContainerInstances({ cluster: clusterArn }).promise()

exports.describeClustersImpl = (ecs, clusterArns) =>
  () => ecs.describeClusters({ clusters: clusterArns }).promise()

exports.describeContainerInstancesImpl = (ecs, clusterArn, containerInstanceArns) =>
  () => ecs.describeContainerInstances({ cluster: clusterArn, containerInstances: containerInstanceArns }).promise()

exports.describeTasksImpl = (ecs, taskArns, clusterArn) =>
  () => ecs.describeTasks({ tasks: taskArns, cluster: clusterArn }).promise()
