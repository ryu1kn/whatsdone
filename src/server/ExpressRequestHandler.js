
'use strict';

const ServiceLocator = require('./ServiceLocator');

class ExpressRequestHandler {

  constructor(params) {
    this._requestProcessor = params.requestProcessor;

    this._expressRequestNormaliser = ServiceLocator.expressRequestNormaliser;
    this._expressResponseSenderFactory = ServiceLocator.expressResponseSenderFactory;
    this._requestProcessErrorProcessor = ServiceLocator.requestProcessErrorProcessor;
    this._authBasedRedirector = ServiceLocator.authBasedRedirector;
  }

  handle(expressReq, expressRes) {
    const normalisedRequest = this._expressRequestNormaliser.normalise(expressReq);
    const normalisedReponse = this._getResponse(normalisedRequest);
    return Promise.resolve(normalisedReponse)
      .catch(e => this._requestProcessErrorProcessor.process(e))
      .then(response => {
        const responseSender = this._expressResponseSenderFactory.create(expressRes);
        return responseSender.send(response);
      });
  }

  _getResponse(request) {
    const redirectResponse = this._authBasedRedirector.redirect(request);
    return redirectResponse || this._requestProcessor.process(request);
  }

}

module.exports = ExpressRequestHandler;
