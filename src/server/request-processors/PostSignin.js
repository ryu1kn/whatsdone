
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class PostSigninRequestProcessor {

  constructor() {
    this._loginCommand = ServiceLocator.loginCommand;
    this._htmlPageGenerator = ServiceLocator.htmlPageGenerator;
    this._cookieCodec = ServiceLocator.cookieCodec;
  }

  process(request) {
    const params = _.pick(request.body, ['email', 'password']);
    return this._loginCommand.execute(params).then(sessionId => {
      if (sessionId) {
        return {
          statusCode: '303',
          headers: {
            'Set-cookie': this._cookieCodec.encode({sessionId}),
            Location: '/'
          }
        };
      }
      return {
        statusCode: '401',
        headers: {
          'Content-Type': 'text/html'
        },
        body: this._htmlPageGenerator.generate('signin')
      };
    });
  }

}

module.exports = PostSigninRequestProcessor;
