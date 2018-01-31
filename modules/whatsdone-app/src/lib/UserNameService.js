
const ServiceLocator = require('./ServiceLocator');

class UserNameService {

  constructor() {
    this._userIdRepository = ServiceLocator.userIdRepository;
    this._cognitoUserFinder = ServiceLocator.cognitoUserFinder;
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
    return this._cognitoUserFinder.find(cognitoUserId)
      .then(user => user && user.Username);
  }

}

module.exports = UserNameService;
