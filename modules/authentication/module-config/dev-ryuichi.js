const { deploymentOutputsBucket } = require('../../../config/common');
const hostName = 'whatsdone-dev-ryuichi';

module.exports = {
  deploymentOutputsBucket,
  identityPoolName: 'Whatsdone Dev Ryuichi',
  userPoolName: hostName,
  callbackUrls: `["https://${hostName}.ryuichi.io"]`
};
