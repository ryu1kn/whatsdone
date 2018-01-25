
const ServiceLocator = require('../ServiceLocator');

class UserRepository {

  constructor() {
    this._userDynamoTableClient = ServiceLocator.userDynamoTableClient;
  }

  getByIds(ids) {
    return this._userDynamoTableClient.getByIds(ids);
  }

}

module.exports = UserRepository;
