
import AWS from 'aws-sdk';
import ServiceLocator from './service-locator';
import {CognitoUser, CognitoUserPool, AuthenticationDetails} from 'amazon-cognito-identity-js';

class Authenticator {

  constructor() {
    this._configProvider = ServiceLocator.configProvider;
  }

  authenticate({email, password}) {
    return this._configProvider.getConfig()
      .then(appConfig => {
        this._configureAWSSdk(appConfig);
        const username = email.substring(0, email.indexOf('@'));
        const userPool = new CognitoUserPool({
          UserPoolId: appConfig.USER_POOL_ID,
          ClientId: appConfig.CLIENT_ID
        });
        const userData = {
          Username: username,
          Pool: userPool
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
        onSuccess: resolve,
        newPasswordRequired: (userAttributes, _requiredAttributes) => {
          cognitoUser.completeNewPasswordChallenge('NEW_DUMMY_PASSWORD', userAttributes, {
            onSuccess: resolve,
            onFailure: reject
          });
        },
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
