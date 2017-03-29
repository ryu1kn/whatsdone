
'use strict';

const ServiceLocator = require('../ServiceLocator');

class GetRootPageRequestProcessor {

  constructor() {
    this._htmlPageGenerator = ServiceLocator.htmlPageGenerator;
  }

  process(_request, _session) {
    return {
      statusCode: '200',
      body: this._htmlPageGenerator.generate('index', {title: 'What\'s done?'}),
      headers: {
        'Content-Type': 'text/html'
      }
    };
  }

}

module.exports = GetRootPageRequestProcessor;
