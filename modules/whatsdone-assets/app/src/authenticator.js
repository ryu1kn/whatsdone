
import AWS from 'aws-sdk';
import ServiceLocator from './service-locator';
import {CognitoUser, CognitoUserPool, AuthenticationDetails} from 'amazon-cognito-identity-js';

class Authenticator {

  constructor() {
    this._configProvider = ServiceLocator.configProvider;
    this._cookieStorage = ServiceLocator.cookieStorage;
  }

  authenticate({username, password}) {
    return this._configProvider.getConfig()
      .then(appConfig => {
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

        const authenticationData = {
          Username: username,
          Password: password
        };
        const authenticationDetails = new AuthenticationDetails(authenticationData);
        return this._authenticate(cognitoUser, authenticationDetails);
      });
  }

  _authenticate(cognitoUser, authenticationDetails) {
    return new Promise((resolve, reject) => {
      cognitoUser.authenticateUser(authenticationDetails, {
        onSuccess: () => resolve({newPasswordRequired: false}),
        newPasswordRequired: (_userAttributes, _requiredAttributes) => resolve({
          newPasswordRequired: true,
          cognitoUser
        }),
        onFailure: reject
      });
    });
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
