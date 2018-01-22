const projectConfig = require('../../../config/dev-ryuichi');

module.exports = {
  certDomainName: '*.ryuichi.io',
  domainName: 'whatsdone-dev-ryuichi-api.ryuichi.io',
  hostedZoneId: projectConfig.hostedZoneId
};
