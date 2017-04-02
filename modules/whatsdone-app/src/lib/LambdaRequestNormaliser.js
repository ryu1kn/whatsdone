
'use strict';

const querystring = require('querystring');

const ServiceLocator = require('./ServiceLocator');

class LambdaRequestNormaliser {

  constructor() {
    this._cookieCodec = ServiceLocator.cookieCodec;
  }

  normalise(event, _lambdaContext) {
    return {
      path: event.path,
      params: event.pathParameters,
      body: querystring.parse(event.body || ''),
      sessionId: this._cookieCodec.extractSessionId(event.headers.Cookie)
    };
  }

}

module.exports = LambdaRequestNormaliser;
