
import WhatsdoneApiClient from './whatsdone-api-client';

class ServiceFactory {

  createSmartFetch() {
    return require('./smart-fetch');
  }

  createWhatsdoneApiClient() {
    return new WhatsdoneApiClient();
  }

}

export default ServiceFactory;
