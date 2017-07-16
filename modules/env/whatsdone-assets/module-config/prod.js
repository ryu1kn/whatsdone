
const projectConfig = require('../../../../config/prod');

module.exports = {
  bucketName: 'whatsdone-assets',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy,
  artifactBucket: projectConfig.artifactBucket
};
