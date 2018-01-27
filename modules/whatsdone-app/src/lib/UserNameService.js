
const ServiceLocator = require('./ServiceLocator');

class UserNameService {

  constructor() {
    this._userIdRepository = ServiceLocator.userIdRepository;
    this._cognitoIdentityServiceProvider = ServiceLocator.cognitoIdentityServiceProvider;
    this._userPoolId = ServiceLocator.config.userPoolId;
  }

  getUsernames(ids) {
    return Promise.all(ids.map(id => this._userIdRepository.getCognitoUserId(id)))
      .then(cognitoUserIds => cognitoUserIds.map((cognitoUserId, index) => cognitoUserId || ids[index]))
      .then(cognitoUserIds => Promise.all(cognitoUserIds.map(id => this._resolveUserName(id))))
      .then(usernames => usernames.map((name, index) => ({
        id: ids[index],
        name
      })));
  }

  _resolveUserName(cognitoUserId) {
    const params = {
      UserPoolId: this._userPoolId,
      AttributesToGet: [],
      Filter: `sub = "${cognitoUserId}"`
    };
    return this._cognitoIdentityServiceProvider.listUsers(params).promise()
      .then(result => result.Users[0])
      .then(user => user && user.Username);
  }

}

module.exports = UserNameService;
