import {ObjectMap} from './models/Collection';

export default class WrappedError extends Error {
  public originalError: Error;
  public details: ObjectMap<any>;

  constructor(originalError, errorDetails?) {
    super(originalError.message);
    this.name = 'WrappedError';
    this.originalError = originalError;
    this.details = errorDetails;
  }

}
