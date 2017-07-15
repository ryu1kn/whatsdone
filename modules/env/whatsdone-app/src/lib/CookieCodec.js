
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

  extractSessionId(cookieString) {
    if (!cookieString) return null;

    const cookieParsed = cookie.parse(cookieString);
    const signedSessionId = cookieParsed['connect.sid'].substr(2);
    return signature.unsign(signedSessionId, this._secret);
  }

}

module.exports = CookieCodec;
