const { deploymentOutputsBucket } = require('../../../config/common');
const hostName = 'whatsdone-prod';

module.exports = {
  deploymentOutputsBucket,
  identityPoolName: 'Whatsdone PROD',
  userPoolName: hostName,
  callbackUrls: `["https://${hostName}.ryuichi.io"]`
};
