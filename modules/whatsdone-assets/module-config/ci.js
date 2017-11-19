
const projectConfig = require('../../../config/ci');

module.exports = {
  bucketName: 'whatsdone-assets-ci',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy,
  artifactBucket: projectConfig.artifactBucket,
  artifactBasePath: 'whatsdone-assets'
};
