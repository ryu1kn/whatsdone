
const WhatsdoneApiClient = require('./WhatsdoneApiClient');

class ServiceFactory {

  createFetchFromWhatsdone() {
    return require('./FetchFromWhatsdone');
  }

  createWhatsdoneApiClient() {
    return new WhatsdoneApiClient();
  }

}

module.exports = ServiceFactory;
