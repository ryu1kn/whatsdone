const { deploymentOutputsBucket, hostedZoneId } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  hostedZoneId,
  certDomainName: '*.ryuichi.io',
  domainName: 'whatsdone-ci-api.ryuichi.io',
};
