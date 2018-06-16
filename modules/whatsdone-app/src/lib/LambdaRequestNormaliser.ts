
import querystring = require('querystring');
import {Claims, Event} from './models/Lambda';
import {ObjectMap} from './models/Collection';

const ContentType = {
  JSON: 'application/json'
};

export type Request = {
  path: string;
  params: ObjectMap<string>,
  query: ObjectMap<string>,
  body: any,
  userInfo: {
    username: string,
    userId: string
  }
};

export default class LambdaRequestNormaliser {

  normalise(event: Event): Request {
    return {
      path: event.path,
      params: event.pathParameters,
      query: event.queryStringParameters || {},
      body: this.parseBody(event.body || '', event.headers['Content-Type']),
      userInfo: this.filterUserInfo(event.requestContext.authorizer.claims)
    };
  }

  private filterUserInfo(claims: Claims) {
    return {
      username: claims['cognito:username'],
      userId: claims.sub
    };
  }

  private parseBody(bodyString: string, contentType: string) {
    return contentType === ContentType.JSON ?
      JSON.parse(bodyString) :
      querystring.parse(bodyString || '');
  }

}
