
'use strict';

const ServiceLocator = require('./ServiceLocator');

class LambdaRequestHandler {

  constructor(params) {
    this._requestProcessor = params.requestProcessor;

    this._sessionRepository = ServiceLocator.sessionRepository;
    this._lambdaRequestNormaliser = ServiceLocator.lambdaRequestNormaliser;
    this._lambdaResponseFormatter = ServiceLocator.lambdaResponseFormatter;
    this._requestProcessErrorProcessor = ServiceLocator.requestProcessErrorProcessor;
    this._authBasedRedirector = ServiceLocator.authBasedRedirector;
  }

  handle(event, context, callback) {
    const normalisedRequest = this._lambdaRequestNormaliser.normalise(event, context);
    const sessionId = normalisedRequest.sessionId;
    const sessionPromise = sessionId ? this._sessionRepository.getById(sessionId) : Promise.resolve();
    return sessionPromise
      .then(session => this._getResponse(normalisedRequest, session))
      .catch(e => this._requestProcessErrorProcessor.process(e))
      .then(response => callback(null, this._lambdaResponseFormatter.format(response)))
      .catch(e => callback(e));
  }

  _getResponse(request, session) {
    const redirectResponse = this._authBasedRedirector.redirect(request, session);
    return redirectResponse || this._requestProcessor.process(request, session);
  }

}

module.exports = LambdaRequestHandler;
