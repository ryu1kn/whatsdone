
'use strict';

const ServiceLocator = require('../ServiceLocator');

class NoMatchingRouteRequestProcessor {

  constructor() {
    this._htmlPageGenerator = ServiceLocator.htmlPageGenerator;
  }

  process(_request) {
    return {
      statusCode: '404',
      headers: {'Content-Type': 'text/html'},
      body: this._htmlPageGenerator.generate('error', {message: '404: Not Found'})
    };
  }

}

module.exports = NoMatchingRouteRequestProcessor;
