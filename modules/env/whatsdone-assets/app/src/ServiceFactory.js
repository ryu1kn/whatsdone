
const WhatsdoneApiClient = require('./WhatsdoneApiClient');

class ServiceFactory {

  createSmartFetch() {
    return require('./smartFetch');
  }

  createWhatsdoneApiClient() {
    return new WhatsdoneApiClient();
  }

}

module.exports = ServiceFactory;
