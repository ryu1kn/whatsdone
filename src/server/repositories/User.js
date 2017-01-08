
const ServiceLocator = require('../ServiceLocator');

class UserRepository {

  constructor() {
    this._userDynamoTableClient = ServiceLocator.userDynamoTableClient;
    this._hashGenerator = ServiceLocator.hashGenerator;
  }

  findUser(loginInfo) {
    return ServiceLocator.userDynamoTableClient.getByQuery({
      email: loginInfo.email,
      password: this._hashGenerator.generate(loginInfo.password)
    });
  }

  getById(id) {
    return this._userDynamoTableClient.getById(id);
  }

  getByIds(ids) {
    return this._userDynamoTableClient.getByIds(ids);
  }

}

module.exports = UserRepository;
