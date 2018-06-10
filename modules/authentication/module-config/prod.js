const { deploymentOutputsBucket } = require('../../../config/common');
const hostName = 'whatsdone';

module.exports = {
  deploymentOutputsBucket,
  identityPoolName: 'Whatsdone PROD',
  userPoolName: `${hostName}-prod`,
  userPoolDomain: hostName,
  callbackUrls: `["https://${hostName}.ryuichi.io"]`
};
