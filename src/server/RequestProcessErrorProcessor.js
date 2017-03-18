
'use strict';

const ServiceLocator = require('./ServiceLocator');

class RequestProcessErrorProcessor {

  constructor() {
    this._logger = ServiceLocator.logger;
    this._htmlPageGenerator = ServiceLocator.htmlPageGenerator;
  }

  process(err) {
    this._logger.error(err.stack);

    // TODO: Instead of having a rule for error message format
    //       to destinguish error types, define custom exception classes
    var parsedInfo = err.message.match(/^\[([^\]]+)]:.*/);
    var errorKind = parsedInfo && parsedInfo[1];

    switch (errorKind) {
    case 'AccessDenied':
      return this._composeResponse('403', '403: Forbidden');

    case 'NotFound':
      return this._composeResponse('404', '404: Not Found');

    default:
      return this._composeResponse(err.status || '500', '500: Internal Server Error');
    }
  }

  _composeResponse(statusCode, errorMessage) {
    return {
      statusCode,
      headers: {'Content-Type': 'text/html'},
      body: this._htmlPageGenerator.generate('error', {message: errorMessage})
    };
  }

}

module.exports = RequestProcessErrorProcessor;
