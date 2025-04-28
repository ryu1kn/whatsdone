#!/usr/bin/env ts-node

import AWS = require('aws-sdk');
import { EnvVars } from './src/lib/EnvVars';
import ServiceFactory from './src/lib/ServiceFactory';
import ServiceLocator from './src/lib/ServiceLocator';

async function main() {
  const input = process.argv[2];
  if (!input) {
    console.error('Usage: yarn ts-node classify-topic.ts "<text to classify>"');
    process.exit(1);
  }

  const accountId = await getAccountId();

  const envVars = new EnvVars({
    USER_POOL_ID: 'DUMMY',
    WEBAPP_ORIGIN: 'DUMMY',
    DB_REGION: 'DUMMY',
    DONE_TABLE_NAME: 'DUMMY',
    USER_ID_TABLE_NAME: 'DUMMY',
    COMPREHEND_REGION: 'ap-southeast-2',
    TOPIC_CLASSIFIER_ARN: `arn:aws:comprehend:ap-southeast-2:${accountId}:document-classifier-endpoint/whatsdone-${process.env.ENV_NAME}-topic-classifier`,
    LOG_LEVEL: 'DEBUG',
  });
  ServiceLocator.load(new ServiceFactory(envVars));

  const classifier = ServiceLocator.topicClassifier;

  const topics = await classifier.classifyText(input);
  console.log('Classified topics:', topics);
}

function getAccountId() {
  return new AWS.STS().getCallerIdentity().promise().then(identity => identity.Account);
}

main();
