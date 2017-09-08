
const projectConfig = require('../../../config/sit');

module.exports = {
  certDomainName: '*.ryuichi.io',
  domainName: 'whatsdone-sit-api.ryuichi.io',
  hostedZoneId: projectConfig.hostedZoneId,
  sessionSecret: 'encrypted::value::AQECAHhxEKFjg5fr5fP9ZzhAmP1EW3Z4kDBqGx2ToIRPQ85N5gAAAHYwdAYJKoZIhvcNAQcGoGcwZQIBADBgBgkqhkiG9w0BBwEwHgYJYIZIAWUDBAEuMBEEDPJlsj7IgQ04iCCgiQIBEIAzlWEEkfi7+/IlOuHXkzLqims8Iep/E/q6tD0cBi8fKlm+F7ImJY4Nr7v9jnmGzgRqPG4z,provider::kms,keyId::alias/whatsdone-app,region::ap-southeast-2'
};
