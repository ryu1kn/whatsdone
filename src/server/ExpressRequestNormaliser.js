
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
      sessionId: this._cookieCodec.extractSessionId(expressReq.cookies)
    };
  }

}

module.exports = ExpressRequestNormaliser;
