const { deploymentOutputsBucket, customDeletionPolicy } = require('../../../config/prod');

module.exports = {
  deploymentOutputsBucket,
  bucketName: 'whatsdone',
  bucketDeletionPolicy: customDeletionPolicy
};
