const { deploymentOutputsBucket } = require('../../../config/common');
const { explicitAuthFlows } = require('./common');
const hostName = 'whatsdone-dev-ryuichi';

module.exports = {
  deploymentOutputsBucket,
  identityPoolName: 'Whatsdone Dev Ryuichi',
  userPoolName: hostName,
  userPoolDomain: hostName,
  callbackUrls: `https://${hostName}.ryuichi.io`,
  explicitAuthFlows
};
