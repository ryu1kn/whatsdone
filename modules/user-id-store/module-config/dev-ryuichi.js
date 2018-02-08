const { deploymentOutputsBucket } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  tableName: 'whatsdone-dev-ryuichi-user-ids',
  readCapacity: '1',
  writeCapacity: '1'
};
