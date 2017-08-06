
const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class PostSigninRequestProcessor {

  constructor() {
    this._loginCommand = ServiceLocator.loginCommand;
    this._cookieCodec = ServiceLocator.cookieCodec;
  }

  process(request, _session) {
    const params = _.pick(request.body, ['email', 'password']);
    return this._loginCommand.execute(params).then(sessionId => {
      if (sessionId) {
        return {
          statusCode: '200',
          headers: {
            'Set-cookie': this._cookieCodec.encode({sessionId}),
            'Content-Type': 'text/plain'
          }
        };
      }
      return {
        statusCode: '401'
      };
    });
  }

}

module.exports = PostSigninRequestProcessor;
