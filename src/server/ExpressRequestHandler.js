
'use strict';

const ServiceLocator = require('./ServiceLocator');

class ExpressRequestHandler {

  constructor(params) {
    this._requestProcessor = params.requestProcessor;

    this._expressRequestNormaliser = ServiceLocator.expressRequestNormaliser;
    this._expressResponseSenderFactory = ServiceLocator.expressResponseSenderFactory;
  }

  handle(expressReq, expressRes) {
    const normalisedRequest = this._expressRequestNormaliser.normalise(expressReq);
    const normalisedReponse = this._requestProcessor.process(normalisedRequest);
    return Promise.resolve(normalisedReponse).then(response => {
      const responseSender = this._expressResponseSenderFactory.create(expressRes);
      return responseSender.send(response);
    });
  }

}

module.exports = ExpressRequestHandler;
