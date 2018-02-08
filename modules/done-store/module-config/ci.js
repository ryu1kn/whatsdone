const { deploymentOutputsBucket } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  tableName: 'whatsdone-ci-dones',
  readCapacity: '1',
  writeCapacity: '1'
};
