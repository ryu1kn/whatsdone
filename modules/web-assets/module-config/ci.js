const projectConfig = require('../../../config/ci');
const moduleCommonConfig = require('./common');

module.exports = Object.assign({}, moduleCommonConfig, {
  bucketName: 'whatsdone-assets-ci',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy
});
