
import ServiceLocator from './service-locator';

class AuthTokenProvider {

  constructor() {
    this._configProvider = ServiceLocator.configProvider;
    this._cookieStorage = ServiceLocator.cookieStorage;
  }

  getIdToken() {
    return this._configProvider.getConfig()
      .then(appConfig => this._readIdTokenFromCookie(appConfig.CLIENT_ID));
  }

  _readIdTokenFromCookie(appClientId) {
    const lastAuthUser = this._cookieStorage.getItem(`CognitoIdentityServiceProvider.${appClientId}.LastAuthUser`);
    return this._cookieStorage.getItem(`CognitoIdentityServiceProvider.${appClientId}.${lastAuthUser}.idToken`);
  }

}

export default AuthTokenProvider;
