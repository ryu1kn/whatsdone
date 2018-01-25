
const ServiceLocator = require('../ServiceLocator');

class UserRepository {

  constructor() {
    this._userDynamoTableClient = ServiceLocator.userDynamoTableClient;
  }

  getById(id) {
    return this._userDynamoTableClient.getById(id);
  }

  getByIds(ids) {
    return this._userDynamoTableClient.getByIds(ids);
  }

}

module.exports = UserRepository;
