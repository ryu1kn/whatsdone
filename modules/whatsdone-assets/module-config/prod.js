const projectConfig = require('../../../config/prod');
const moduleCommonConfig = require('./common');

module.exports = Object.assign({}, moduleCommonConfig, {
  bucketName: 'whatsdone-assets',
  bucketDeletionPolicy: projectConfig.customDeletionPolicy
});
