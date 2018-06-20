import ServiceLocator from './ServiceLocator';
import UserIdRepository from './repositories/UserId';
import LambdaRequestNormaliser from './LambdaRequestNormaliser';
import LambdaResponseFormatter from './LambdaResponseFormatter';
import RequestProcessErrorProcessor from './RequestProcessErrorProcessor';
import {RequestProcessor} from './RequestProcessor';
import {Event} from './models/Lambda';
import {Response} from './models/Request';

export type Session = {
  userId?: string;
  username?: string;
};

export default class LambdaRequestHandler {
  private _requestProcessor: RequestProcessor;
  private _userIdRepository: UserIdRepository;
  private _lambdaRequestNormaliser: LambdaRequestNormaliser;
  private _lambdaResponseFormatter: LambdaResponseFormatter;
  private _requestProcessErrorProcessor: RequestProcessErrorProcessor;

  constructor(requestProcessor: RequestProcessor) {
    this._requestProcessor = requestProcessor;

    this._userIdRepository = ServiceLocator.userIdRepository;
    this._lambdaRequestNormaliser = ServiceLocator.lambdaRequestNormaliser;
    this._lambdaResponseFormatter = ServiceLocator.lambdaResponseFormatter;
    this._requestProcessErrorProcessor = ServiceLocator.requestProcessErrorProcessor;
  }

  handle = async (event: Event): Promise<Response> => {
    const response = await this.handleRequest(event);
    return this._lambdaResponseFormatter.format(response);
  }

  private async handleRequest(event: Event) {
    const normalisedRequest = this._lambdaRequestNormaliser.normalise(event);
    const cognitoUserId = normalisedRequest.userInfo.userId;
    const username = normalisedRequest.userInfo.username;
    try {
      const session = {
        userId: await this._userIdRepository.getByCognitoUserId(cognitoUserId),
        username
      };
      return await this._requestProcessor.process(normalisedRequest, session);
    } catch (e) {
      return this._requestProcessErrorProcessor.process(e);
    }
  }

}
