const { deploymentOutputsBucket, customDeletionPolicy } = require('../../../config/dev-ryuichi');

module.exports = {
  deploymentOutputsBucket,
  bucketName: 'whatsdone-dev-ryuichi',
  bucketDeletionPolicy: customDeletionPolicy
};
