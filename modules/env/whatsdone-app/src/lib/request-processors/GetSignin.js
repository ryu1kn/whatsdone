
'use strict';

const ServiceLocator = require('../ServiceLocator');

class GetSigninRequestProcessor {

  constructor() {
    this._htmlPageGenerator = ServiceLocator.htmlPageGenerator;
  }

  process(_request, _session) {
    return {
      statusCode: '200',
      body: this._htmlPageGenerator.generate('signin', {title: 'Sign In - What\'s done?'}),
      headers: {
        'Content-Type': 'text/html'
      }
    };
  }

}

module.exports = GetSigninRequestProcessor;
