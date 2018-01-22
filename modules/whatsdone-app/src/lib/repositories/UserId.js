
const ServiceLocator = require('../ServiceLocator');

class UserIdRepository {

  constructor() {
    this._userIdDynamoTableClient = ServiceLocator.userIdDynamoTableClient;
  }

  getUserId(cognitoUserId) {
    return this._userIdDynamoTableClient.getById(cognitoUserId)
      .then(result => result && result.userId);
  }

}

module.exports = UserIdRepository;
