
const projectConfig = require('../../../config/prod');

module.exports = {
  bucketName: 'whatsdone',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy
};
