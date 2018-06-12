import AWS from 'aws-sdk';
import ServiceLocator from './service-locator';
import {parse} from 'querystring';
import jwtDecode from 'jwt-decode';
import {
  CognitoAccessToken,
  CognitoIdToken,
  CognitoRefreshToken,
  CognitoUser,
  CognitoUserPool,
  CognitoUserSession
} from 'amazon-cognito-identity-js';

class CognitoUserInitialiser {

  constructor() {
    this._configProvider = ServiceLocator.configProvider;
    this._cookieStorage = ServiceLocator.cookieStorage;
  }

  async initialise() {
    const appConfig = await this._configProvider.getConfig();
    this._configureAWSSdk(appConfig);
    const userPool = new CognitoUserPool({
      UserPoolId: appConfig.USER_POOL_ID,
      ClientId: appConfig.CLIENT_ID,
      Storage: this._cookieStorage
    });

    const hash = window.location.hash.substr(1);
    const token = parse(hash);

    if (token.id_token && token.access_token) {
      const userData = {
        Username: jwtDecode(token.id_token)['cognito:username'],
        Pool: userPool,
        Storage: this._cookieStorage
      };
      const cognitoUser = new CognitoUser(userData);
      const userSession = new CognitoUserSession({
        IdToken: new CognitoIdToken({IdToken: token.id_token}),
        AccessToken: new CognitoAccessToken({AccessToken: token.access_token}),
        RefreshToken: new CognitoRefreshToken()
      });
      cognitoUser.setSignInUserSession(userSession);
    }
  }

  _configureAWSSdk(appConfig) {
    AWS.config.region = appConfig.REGION;
    AWS.config.credentials = new AWS.CognitoIdentityCredentials({
      IdentityPoolId: appConfig.IDENTITY_POOL_ID
    });
  }

}

module.exports = CognitoUserInitialiser;