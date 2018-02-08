const { deploymentOutputsBucket } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  identityPoolName: 'Whatsdone CI',
  userPoolName: 'whatsdone-ci'
};
