
'use strict';

const querystring = require('querystring');

const ContentType = {
  JSON: 'application/json'
};

class LambdaRequestNormaliser {

  normalise(event, _lambdaContext) {
    return {
      path: event.path,
      params: event.pathParameters,
      query: event.queryStringParameters || {},
      body: this._parseBody(event.body, event.headers['Content-Type']),
      userInfo: this._filterUserInfo(event.requestContext.authorizer.claims)
    };
  }

  _filterUserInfo(claims) {
    return {
      username: claims['cognito:username'],
      userId: claims.sub
    };
  }

  _parseBody(bodyString, contentType) {
    return contentType === ContentType.JSON ?
      JSON.parse(bodyString) :
      querystring.parse(bodyString || '');
  }

}

module.exports = LambdaRequestNormaliser;
