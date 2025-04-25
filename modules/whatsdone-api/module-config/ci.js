const { deploymentOutputsBucket, hostedZoneId } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  hostedZoneId,
  domainName: 'whatsdone-ci-api.ryuichi.io',
  topicClassifierName: 'whatsdone-topic-classifier',
};
