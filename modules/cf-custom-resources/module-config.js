
const { deploymentOutputsBucket, artifactBucket } = require('../../config/common');

module.exports = {
  deploymentOutputsBucket,
  artifactBucket,
  artifactBasePath: 'cf-custom-resources'
};
