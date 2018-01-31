
const ServiceLocator = require('./ServiceLocator');

class CognitoUserFinder {

  constructor() {
    this._cognitoIdentityServiceProvider = ServiceLocator.cognitoIdentityServiceProvider;
    this._userPoolId = ServiceLocator.config.userPoolId;
  }

  find(cognitoUserId) {
    const params = {
      UserPoolId: this._userPoolId,
      AttributesToGet: [],
      Filter: `sub = "${cognitoUserId}"`
    };
    return this._cognitoIdentityServiceProvider.listUsers(params).promise()
      .then(result => result.Users[0]);
  }

}

module.exports = CognitoUserFinder;
