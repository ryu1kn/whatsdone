
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

  async handle(event, context, callback) {
    try {
      const response = await this._handleRequest(event, context);
      callback(null, this._lambdaResponseFormatter.format(response));
    } catch (e) {
      callback(e);
    }
  }

  async _handleRequest(event, context) {
    const normalisedRequest = this._lambdaRequestNormaliser.normalise(event, context);
    const cognitoUserId = normalisedRequest.userInfo.userId;
    const username = normalisedRequest.userInfo.username;
    try {
      const userId = await this._userIdRepository.getByCognitoUserId(cognitoUserId);
      return await this._requestProcessor.process(normalisedRequest, {userId, username});
    } catch (e) {
      return await this._requestProcessErrorProcessor.process(e);
    }
  }

}

module.exports = LambdaRequestHandler;
