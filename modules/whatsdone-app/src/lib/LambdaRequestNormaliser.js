
'use strict';

const querystring = require('querystring');

const ServiceLocator = require('./ServiceLocator');

const ContentType = {
  JSON: 'application/json'
};

class LambdaRequestNormaliser {

  constructor() {
    this._cookieCodec = ServiceLocator.cookieCodec;
  }

  normalise(event, _lambdaContext) {
    return {
      path: event.path,
      params: event.pathParameters,
      query: event.queryStringParameters || {},
      body: this._parseBody(event.body, event.headers['Content-Type']),
      sessionId: this._cookieCodec.extractSessionId(event.headers.Cookie)
    };
  }

  _parseBody(bodyString, contentType) {
    return contentType === ContentType.JSON ?
        JSON.parse(bodyString) :
        querystring.parse(bodyString || '');
  }

}

module.exports = LambdaRequestNormaliser;
