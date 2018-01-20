
import Authenticator from './authenticator';
import ConfigProvider from './config-provider';
import WhatsdoneApiClient from './whatsdone-api-client';

class ServiceFactory {

  createAuthenticator() {
    return new Authenticator();
  }

  createFetch() {
    // HACK: fetch/webpack combination problem? cf. https://github.com/developit/unfetch/issues/46
    return fetch.bind();
  }

  createConfigProvider() {
    return new ConfigProvider();
  }

  createSmartFetch() {
    return require('./smart-fetch');
  }

  createWhatsdoneApiClient() {
    return new WhatsdoneApiClient();
  }

}

export default ServiceFactory;
