
'use strict';

const cookie = require('cookie');
const signature = require('cookie-signature');

class CookieCodec {

  constructor(params) {
    this._secret = params.signatureSecret;
  }

  encode(params) {
    const signedSessionId = signature.sign(params.sessionId, this._secret);
    const cookieOption = {
      httpOnly: true,
      path: '/'
    };
    return cookie.serialize('connect.sid', `s:${signedSessionId}`, cookieOption);
  }

}

module.exports = CookieCodec;
