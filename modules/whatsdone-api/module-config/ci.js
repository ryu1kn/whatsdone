const { deploymentOutputsBucket, hostedZoneId, nodeVersionMajor } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  hostedZoneId,
  domainName: 'whatsdone-ci-api.ryuichi.io',
  nodeVersionMajor: `${nodeVersionMajor}`,
};
