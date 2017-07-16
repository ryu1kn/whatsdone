
const projectConfig = require('../../../../config/sit');

module.exports = {
  bucketName: 'whatsdone-assets-sit',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy,
  artifactBucket: projectConfig.artifactBucket,
  artifactBasePath: 'whatsdone-assets'
};
