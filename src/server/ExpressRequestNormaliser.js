
'use strict';

class ExpressRequestNormaliser {

  normalise(expressReq) {
    return {
      params: expressReq.params,
      body: expressReq.body,
      session: expressReq.session
    };
  }

}

module.exports = ExpressRequestNormaliser;
