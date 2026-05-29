
import {CognitoIdentityProviderClient, ListUsersCommand} from '@aws-sdk/client-cognito-identity-provider';
import ServiceLocator from './ServiceLocator';

export type CognitoUser = {
  Username: string;
};

export default class CognitoUserFinder {
  private _cognitoIdentityServiceProvider: CognitoIdentityProviderClient;
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
    const result = await this._cognitoIdentityServiceProvider.send(new ListUsersCommand(params));
    return result.Users![0] as unknown as CognitoUser;
  }

}
