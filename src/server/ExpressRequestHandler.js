
'use strict';

const ServiceLocator = require('./ServiceLocator');

class ExpressRequestHandler {

  constructor(params) {
    this._requestProcessor = params.requestProcessor;

    this._expressRequestNormaliser = ServiceLocator.expressRequestNormaliser;
    this._sessionLoader = ServiceLocator.sessionLoader;
    this._expressResponseSenderFactory = ServiceLocator.expressResponseSenderFactory;
    this._requestProcessErrorProcessor = ServiceLocator.requestProcessErrorProcessor;
    this._authBasedRedirector = ServiceLocator.authBasedRedirector;
  }

  handle(expressReq, expressRes) {
    const normalisedRequest = this._expressRequestNormaliser.normalise(expressReq);
    return this._sessionLoader.load(normalisedRequest.sessionId)
      .then(session => this._getResponse(normalisedRequest, session))
      .catch(e => this._requestProcessErrorProcessor.process(e))
      .then(response => {
        const responseSender = this._expressResponseSenderFactory.create(expressRes);
        return responseSender.send(response);
      });
  }

  _getResponse(request, session) {
    const redirectResponse = this._authBasedRedirector.redirect(request, session);
    return redirectResponse || this._requestProcessor.process(request, session);
  }

}

module.exports = ExpressRequestHandler;
