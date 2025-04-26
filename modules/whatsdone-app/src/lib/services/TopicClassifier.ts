import AWS = require('aws-sdk');
import ServiceLocator from '../ServiceLocator';
import { Logger } from '../Logger';

export default class TopicClassifier {
  private _comprehend: AWS.Comprehend;
  private _classifierArn: string;
  private _logger: Logger;

  constructor(classifier_arn: string) {
    this._comprehend = ServiceLocator.comprehendClient;
    this._classifierArn = classifier_arn;
    this._logger = ServiceLocator.logger;
  }

  async classifyText(text: string): Promise<string[]> {
    const result = await this._comprehend.classifyDocument({
      Text: text,
      EndpointArn: this._classifierArn
    }).promise();

    const topicsWithConfidence = (result.Classes ?? []).map(c => ({topic: c.Name!, confidence: c.Score!}));

    this._logger.debug(`Topics with confidence: ${JSON.stringify(topicsWithConfidence)}`);

    return topicsWithConfidence.map(c => c.topic);
  }
}
