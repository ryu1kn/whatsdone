
const projectConfig = require('../../../config/prod');

module.exports = {
  certDomainName: '*.ryuichi.io',
  domainName: 'whatsdone-api.ryuichi.io',
  hostedZoneId: projectConfig.hostedZoneId
};
