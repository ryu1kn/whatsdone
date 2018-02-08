const { deploymentOutputsBucket } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  tableName: 'whatsdone-prod-dones',
  readCapacity: '1',
  writeCapacity: '1'
};
