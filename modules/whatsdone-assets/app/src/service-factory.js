import AuthTokenProvider from './auth-token-provider';
import {CognitoUserInitialiser} from './cognito-user-initialiser';
import ConfigProvider from './config-provider';
import WhatsdoneApiClient from './whatsdone-api-client';
import {CookieStorage} from 'amazon-cognito-identity-js';
import smartFetch from './smart-fetch';

class ServiceFactory {

  createAuthTokenProvider() {
    return new AuthTokenProvider();
  }

  createFetch() {
    // HACK: fetch/webpack combination problem? cf. https://github.com/developit/unfetch/issues/46
    return fetch.bind();
  }

  createCognitoUserInitialiser() {
    return new CognitoUserInitialiser();
  }

  createConfigProvider() {
    return new ConfigProvider();
  }

  createCookieStorage() {
    return new CookieStorage({domain: `.${location.host}`});
  }

  createSmartFetch() {
    return smartFetch;
  }

  createWhatsdoneApiClient() {
    return new WhatsdoneApiClient();
  }

}

export default ServiceFactory;
