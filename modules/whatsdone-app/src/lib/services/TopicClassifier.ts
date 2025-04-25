import AWS = require('aws-sdk');
import ServiceLocator from '../ServiceLocator';

export default class TopicClassifier {
  private _comprehend: AWS.Comprehend;
  private _classifierArn: string;

  constructor(classifier_arn: string) {
    this._comprehend = ServiceLocator.comprehendClient;
    this._classifierArn = classifier_arn;
  }

  async classifyText(text: string): Promise<string[]> {
    try {
      const result = await this._comprehend.classifyDocument({
        Text: text,
        EndpointArn: this._classifierArn
      }).promise();

      return result.Classes?.map(c => c.Name || '') || [];
    } catch (error) {
      console.error('Failed to classify text:', error);
      return [];
    }
  }
}
