
import WhatsdoneApiClient from './whatsdone-api-client';

class ServiceFactory {

  createFetch() {
    // HACK: fetch/webpack combination problem? cf. https://github.com/developit/unfetch/issues/46
    return fetch.bind();
  }

  createSmartFetch() {
    return require('./smart-fetch');
  }

  createWhatsdoneApiClient() {
    return new WhatsdoneApiClient();
  }

}

export default ServiceFactory;
