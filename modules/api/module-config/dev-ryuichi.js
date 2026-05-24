const { deploymentOutputsBucket, hostedZoneId, nodeVersionMajor } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  hostedZoneId,
  domainName: 'whatsdone-dev-ryuichi-api.ryuichi.io',
  nodeVersionMajor: `${nodeVersionMajor}`,
};
