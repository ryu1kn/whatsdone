
import querystring = require('querystring');
import {Event} from './models/Lambda';

const ContentType = {
  JSON: 'application/json'
};

export default class LambdaRequestNormaliser {

  normalise(event: Event) {
    return {
      path: event.path,
      params: event.pathParameters,
      query: event.queryStringParameters || {},
      body: this.parseBody(event.body, event.headers['Content-Type']),
      userInfo: this.filterUserInfo(event.requestContext.authorizer.claims)
    };
  }

  private filterUserInfo(claims) {
    return {
      username: claims['cognito:username'],
      userId: claims.sub
    };
  }

  private parseBody(bodyString, contentType) {
    return contentType === ContentType.JSON ?
      JSON.parse(bodyString) :
      querystring.parse(bodyString || '');
  }

}
