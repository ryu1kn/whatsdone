
import ServiceLocator from './ServiceLocator';

export default class CognitoUserFinder {
  private _cognitoIdentityServiceProvider: any;
  private _userPoolId: any;

  constructor() {
    this._cognitoIdentityServiceProvider = ServiceLocator.cognitoIdentityServiceProvider;
    this._userPoolId = ServiceLocator.config.userPoolId;
  }

  async find(cognitoUserId) {
    const params = {
      UserPoolId: this._userPoolId,
      AttributesToGet: [],
      Filter: `sub = "${cognitoUserId}"`
    };
    const result = await this._cognitoIdentityServiceProvider.listUsers(params).promise();
    return result.Users[0];
  }

}
