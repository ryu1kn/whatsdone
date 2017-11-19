const projectConfig = require('../../../config/dev-ryuichi');

module.exports = {
  bucketName: 'whatsdone-dev-ryuichi',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy
};
