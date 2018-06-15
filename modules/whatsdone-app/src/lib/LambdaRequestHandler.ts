import ServiceLocator from './ServiceLocator';
import UserIdRepository from './repositories/UserId';
import LambdaRequestNormaliser from './LambdaRequestNormaliser';
import LambdaResponseFormatter from './LambdaResponseFormatter';
import RequestProcessErrorProcessor from './RequestProcessErrorProcessor';
import {RequestProcessor} from './RequestProcessor';
import {Event} from './models/Lambda';

export default class LambdaRequestHandler {
  private _requestProcessor: RequestProcessor;
  private _userIdRepository: UserIdRepository;
  private _lambdaRequestNormaliser: LambdaRequestNormaliser;
  private _lambdaResponseFormatter: LambdaResponseFormatter;
  private _requestProcessErrorProcessor: RequestProcessErrorProcessor;

  constructor(params) {
    this._requestProcessor = params.requestProcessor;

    this._userIdRepository = ServiceLocator.userIdRepository;
    this._lambdaRequestNormaliser = ServiceLocator.lambdaRequestNormaliser;
    this._lambdaResponseFormatter = ServiceLocator.lambdaResponseFormatter;
    this._requestProcessErrorProcessor = ServiceLocator.requestProcessErrorProcessor;
  }

  handle = async (event: Event, callback): Promise<void> => {
    try {
      const response = await this.handleRequest(event);
      callback(null, this._lambdaResponseFormatter.format(response));
    } catch (e) {
      callback(e);
    }
  }

  private async handleRequest(event) {
    const normalisedRequest = this._lambdaRequestNormaliser.normalise(event);
    const cognitoUserId = normalisedRequest.userInfo.userId;
    const username = normalisedRequest.userInfo.username;
    try {
      const userId = await this._userIdRepository.getByCognitoUserId(cognitoUserId);
      return await this._requestProcessor.process(normalisedRequest, {userId, username});
    } catch (e) {
      return this._requestProcessErrorProcessor.process(e);
    }
  }

}
