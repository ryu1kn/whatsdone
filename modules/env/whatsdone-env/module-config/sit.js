
const projectConfig = require('../../../../config/sit');

module.exports = {
  bucketName: 'whatsdone-sit',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy
};
