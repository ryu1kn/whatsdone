const { deploymentOutputsBucket } = require('../../../config/common');
const { explicitAuthFlows } = require('./common');
const hostName = 'whatsdone-ci';

module.exports = {
  deploymentOutputsBucket,
  identityPoolName: 'Whatsdone CI',
  userPoolName: hostName,
  userPoolDomain: hostName,
  callbackUrls: `["https://${hostName}.ryuichi.io"]`,
  explicitAuthFlows: [explicitAuthFlows, 'ALLOW_USER_PASSWORD_AUTH'].join(',')
};
