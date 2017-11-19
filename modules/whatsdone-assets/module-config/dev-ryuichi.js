const projectConfig = require('../../../config/dev-ryuichi');

module.exports = {
  bucketName: 'whatsdone-assets-dev-ryuichi',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy,
  artifactBucket: projectConfig.artifactBucket,
  artifactBasePath: 'whatsdone-assets'
};
