
'use strict';

class ExpressRequestNormaliser {

  normalise(expressReq) {
    return {
      path: expressReq.path,
      params: expressReq.params,
      body: expressReq.body,
      session: expressReq.session
    };
  }

}

module.exports = ExpressRequestNormaliser;
