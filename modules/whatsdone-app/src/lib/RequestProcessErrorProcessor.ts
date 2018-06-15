
import ServiceLocator from './ServiceLocator';
import {Logger} from './Logger';

export default class RequestProcessErrorProcessor {
  private _logger: Logger;

  constructor() {
    this._logger = ServiceLocator.logger;
  }

  process(err) {
    this._logger.error(err.stack, {details: err.details && JSON.stringify(err.details)});

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
      return this.composeResponse(err.status || '500', '500: Internal Server Error');
    }
  }

  private composeResponse(statusCode, errorMessage) {
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
