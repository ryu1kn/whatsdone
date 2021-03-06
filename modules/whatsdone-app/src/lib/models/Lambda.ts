import {ObjectMap} from './Collection';

export type Event = {
  httpMethod: string,
  path: string,
  pathParameters: ObjectMap<string>,
  queryStringParameters?: ObjectMap<string>,
  body?: string,
  headers: ObjectMap<string>,
  requestContext: {
    authorizer: {
      claims: Claims
    }
  }
};

export type Claims = {
  'cognito:username': string;
  sub: string;
};
