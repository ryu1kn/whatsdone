const projectConfig = require('../../../config/dev-ryuichi');
const moduleCommonConfig = require('./common');

module.exports = Object.assign({}, moduleCommonConfig, {
  bucketName: 'whatsdone-assets-dev-ryuichi',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy
});
