import {ObjectMap} from './Collection';

export type Response = {
  statusCode: string;
  headers?: ObjectMap<string>;
  body?: string;
};
