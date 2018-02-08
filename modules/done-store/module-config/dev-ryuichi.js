const { deploymentOutputsBucket } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  tableName: 'whatsdone-dev-ryuichi-dones',
  readCapacity: '1',
  writeCapacity: '1'
};
