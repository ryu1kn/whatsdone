const projectConfig = require('../../../config/ci');

module.exports = {
  certDomainName: '*.ryuichi.io',
  domainName: 'whatsdone-ci-api.ryuichi.io',
  hostedZoneId: projectConfig.hostedZoneId
};
