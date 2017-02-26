
'use strict';

class ExpressResponseSender {

  constructor(params) {
    this._expressRes = params.expressRes;
  }

  send(response) {
    this._expressRes.status(response.statusCode);
    this._expressRes.set(response.headers);
    this._expressRes.send(response.body);
  }

}

module.exports = ExpressResponseSender;
