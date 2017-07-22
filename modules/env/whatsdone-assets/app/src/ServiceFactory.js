
class ServiceFactory {

  createFetchFromWhatsdone() {
    return require('./FetchFromWhatsdone');
  }

  createWhatsdoneApiClient() {
    const WhatsdoneApiClient = require('./whatsdoneApiClient');
    return new WhatsdoneApiClient();
  }

}

module.exports = ServiceFactory;
