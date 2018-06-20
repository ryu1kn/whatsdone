
import ServiceLocator from './ServiceLocator';
import {Logger} from './Logger';
import WrappedError from './WrappedError';

export default class RequestProcessErrorProcessor {
  private _logger: Logger;

  constructor() {
    this._logger = ServiceLocator.logger;
  }

  process(err: Error) {
    const details = err instanceof WrappedError ? {details: err.details && JSON.stringify(err.details)} : undefined;
    this._logger.error(err.stack, details);

    // TODO: Instead of having a rule for error message format
    //       to destinguish error types, define custom exception classes
    var parsedInfo = err.message.match(/^\[([^\]]+)]:.*/);
    var errorKind = parsedInfo && parsedInfo[1];

    switch (errorKind) {
    case 'AccessDenied':
      return this.composeResponse('403', '403: Forbidden');

    case 'NotFound':
      return this.composeResponse('404', '404: Not Found');

    default:
      return this.composeResponse('500', '500: Internal Server Error');
    }
  }

  private composeResponse(statusCode: string, errorMessage: string) {
    const errorResponse = [
      {title: errorMessage}
    ];
    return {
      statusCode,
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: errorResponse})
    };
  }

}
