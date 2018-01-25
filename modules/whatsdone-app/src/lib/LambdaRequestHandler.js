
'use strict';

const ServiceLocator = require('./ServiceLocator');

class LambdaRequestHandler {

  constructor(params) {
    this._requestProcessor = params.requestProcessor;

    this._userIdRepository = ServiceLocator.userIdRepository;
    this._lambdaRequestNormaliser = ServiceLocator.lambdaRequestNormaliser;
    this._lambdaResponseFormatter = ServiceLocator.lambdaResponseFormatter;
    this._requestProcessErrorProcessor = ServiceLocator.requestProcessErrorProcessor;
  }

  handle(event, context, callback) {
    const normalisedRequest = this._lambdaRequestNormaliser.normalise(event, context);
    const cognitoUserId = normalisedRequest.userInfo.userId;
    const username = normalisedRequest.userInfo.username;
    const userIdPromise = this._userIdRepository.getByCognitoUserId(cognitoUserId);
    return userIdPromise
      .then(userId => this._requestProcessor.process(normalisedRequest, {userId, username}))
      .catch(e => this._requestProcessErrorProcessor.process(e))
      .then(response => callback(null, this._lambdaResponseFormatter.format(response)))
      .catch(e => callback(e));
  }

}

module.exports = LambdaRequestHandler;
