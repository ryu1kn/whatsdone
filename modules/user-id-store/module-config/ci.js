const { deploymentOutputsBucket } = require('../../../config/common');

module.exports = {
  deploymentOutputsBucket,
  tableName: 'whatsdone-ci-user-ids',
  readCapacity: '1',
  writeCapacity: '1'
};
