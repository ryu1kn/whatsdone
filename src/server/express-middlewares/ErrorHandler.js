
'use strict';

const ServiceLocator = require('../ServiceLocator');

class ErrorHandler {

  constructor() {
    this._logger = ServiceLocator.logger;
  }

  handle(err, req, res, _next) {
    this._logger.error(err.stack);

    // TODO: Instead of having a rule for error message format
    //       to destinguish error types, define custom exception classes
    var parsedInfo = err.message.match(/^\[([^\]]+)]:.*/);
    var errorKind = parsedInfo && parsedInfo[1];
    var clientMessage;

    switch (errorKind) {
    case 'AccessDenied':
      res.status(403);
      clientMessage = '403: Forbidden';
      break;

    case 'NotFound':
      res.status(404);
      clientMessage = '404: Not Found';
      break;

    default:
      res.status(err.status || 500);
      clientMessage = '500: Internal Server Error';
    }
    res.render('error', {message: clientMessage});
  }

}

module.exports = ErrorHandler;
