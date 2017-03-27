
'use strict';

const ServiceLocator = require('./ServiceLocator');

class ExpressRequestNormaliser {

  constructor() {
    this._cookieCodec = ServiceLocator.cookieCodec;
  }

  normalise(expressReq) {
    return {
      path: expressReq.path,
      params: expressReq.params,
      body: expressReq.body,
      sessionId: this._cookieCodec.extractSessionId(expressReq.get('cookie'))
    };
  }

}

module.exports = ExpressRequestNormaliser;
