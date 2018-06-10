const { deploymentOutputsBucket } = require('../../../config/common');
const hostName = 'whatsdone-ci';

module.exports = {
  deploymentOutputsBucket,
  identityPoolName: 'Whatsdone CI',
  userPoolName: hostName,
  userPoolDomain: hostName,
  callbackUrls: `["https://${hostName}.ryuichi.io"]`
};
