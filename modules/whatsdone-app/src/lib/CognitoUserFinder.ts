
import ServiceLocator from './ServiceLocator';

export type CognitoUser = {
  Username: string;
};

export default class CognitoUserFinder {
  private _cognitoIdentityServiceProvider: AWS.CognitoIdentityServiceProvider;
  private _userPoolId: string;

  constructor() {
    this._cognitoIdentityServiceProvider = ServiceLocator.cognitoIdentityServiceProvider;
    this._userPoolId = ServiceLocator.config.userPoolId;
  }

  async find(cognitoUserId: string) {
    const params = {
      UserPoolId: this._userPoolId,
      AttributesToGet: [],
      Filter: `sub = "${cognitoUserId}"`
    };
    const result = await this._cognitoIdentityServiceProvider.listUsers(params).promise();
    return result.Users![0] as CognitoUser;
  }

}
