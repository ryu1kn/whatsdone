const { deploymentOutputsBucket, customDeletionPolicy } = require('../../../config/ci');

module.exports = {
  deploymentOutputsBucket,
  bucketName: 'whatsdone-ci',
  bucketDeletionPolicy: customDeletionPolicy
};
