const projectConfig = require('../../../config/ci');

module.exports = {
  bucketName: 'whatsdone-ci',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy
};
