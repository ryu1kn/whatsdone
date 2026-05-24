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
  CognitoUserSession,
  CookieStorage
} from 'amazon-cognito-identity-js';
import ConfigProvider, {AppConfig} from './config-provider';

interface JwtToken {
  id_token: string
  access_token: string
  expires_in: string
  token_type: string
}

export class CognitoUserInitialiser {
  private readonly _configProvider: ConfigProvider;
  private readonly _cookieStorage: CookieStorage;

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
    const token = parse(hash) as unknown as JwtToken;

    if (token.id_token && token.access_token) {
      const userData = {
        Username: jwtDecode<{'cognito:username': string}>(token.id_token)['cognito:username'],
        Pool: userPool,
        Storage: this._cookieStorage
      };
      const cognitoUser = new CognitoUser(userData);
      const userSession = new CognitoUserSession({
        IdToken: new CognitoIdToken({IdToken: token.id_token}),
        AccessToken: new CognitoAccessToken({AccessToken: token.access_token}),
        // @ts-ignore
        RefreshToken: new CognitoRefreshToken()
      });
      cognitoUser.setSignInUserSession(userSession);
    }
  }

  private _configureAWSSdk(appConfig: AppConfig) {
    AWS.config.region = appConfig.REGION;
    AWS.config.credentials = new AWS.CognitoIdentityCredentials({
      IdentityPoolId: appConfig.IDENTITY_POOL_ID
    });
  }
}
