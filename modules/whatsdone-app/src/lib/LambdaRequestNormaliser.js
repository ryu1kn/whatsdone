
'use strict';

const ServiceLocator = require('./ServiceLocator');

class LambdaRequestNormaliser {

  constructor() {
    this._cookieCodec = ServiceLocator.cookieCodec;
  }

  normalise(event, _lambdaContext) {
    return {
      path: expressReq.path,
      params: expressReq.params,
      body: expressReq.body,
      sessionId: this._cookieCodec.extractSessionId(expressReq.get('cookie'))
    };
  }

}

module.exports = LambdaRequestNormaliser;
