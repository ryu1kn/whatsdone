import AWS from 'aws-sdk';
import ServiceLocator from './service-locator';
import {CognitoUser, CognitoUserPool, CognitoUserSession} from 'amazon-cognito-identity-js';
import {parse} from 'querystring';

class Authenticator {

  constructor() {
    this._configProvider = ServiceLocator.configProvider;
    this._cookieStorage = ServiceLocator.cookieStorage;
  }

  async authenticate({username}) {
    const appConfig = await this._configProvider.getConfig();
    this._configureAWSSdk(appConfig);
    const userPool = new CognitoUserPool({
      UserPoolId: appConfig.USER_POOL_ID,
      ClientId: appConfig.CLIENT_ID,
      Storage: this._cookieStorage
    });
    const userData = {
      Username: username,
      Pool: userPool,
      Storage: this._cookieStorage
    };
    const cognitoUser = new CognitoUser(userData);

    const hash = window.location.hash.substr(1);
    const token = parse(hash);

    const userSession = new CognitoUserSession({
      IdToken: token.id_token,
      AccessToken: token.access_token
    });
    cognitoUser.setSignInUserSession(userSession);
  }

  updatePassword(cognitoUser, newPassword) {
    const userAttributes = {};
    return new Promise((resolve, reject) => {
      cognitoUser.completeNewPasswordChallenge(newPassword, userAttributes, {
        onSuccess: resolve(),
        onFailure: reject
      });
    });
  }

  _configureAWSSdk(appConfig) {
    AWS.config.region = appConfig.REGION;
    AWS.config.credentials = new AWS.CognitoIdentityCredentials({
      IdentityPoolId: appConfig.IDENTITY_POOL_ID
    });
  }

}

export default Authenticator;
