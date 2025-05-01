import ServiceLocator from '../../lib/ServiceLocator';
import TopicClassifier from '../../lib/services/TopicClassifier';
import {deepStrictEqual} from 'assert';
import * as td from 'testdouble';
import AWS = require('aws-sdk');
import ServiceFactory from '../../lib/ServiceFactory';

describe('TopicClassifier', () => {
  const classifierArn = 'test-classifier-arn';
  const sampleText = 'Sample text to classify';

  let comprehendClient: AWS.Comprehend;
  let logger: { debug: (message: string) => void };

  beforeEach(() => {
    // Create a mock Comprehend client with the required method
    comprehendClient = {
      classifyDocument: () => ({}) // This will be overridden by td.when
    } as any;
    td.replace(comprehendClient, 'classifyDocument');

    logger = { debug: td.func<(message: string) => void>() };

    const factory = {
      createComprehendClient: () => comprehendClient,
      createLogger: () => logger
    } as ServiceFactory;

    ServiceLocator.load(factory);
  });

  afterEach(() => {
    td.reset();
  });

  it('classifies text and returns topics', async () => {
    const mockResponse = {
      Classes: [
        { Name: 'Topic1', Score: 0.9 },
        { Name: 'Topic2', Score: 0.8 }
      ]
    } as AWS.Comprehend.ClassifyDocumentResponse;

    td.when(comprehendClient.classifyDocument({
      Text: sampleText,
      EndpointArn: classifierArn
    })).thenReturn({
      promise: () => Promise.resolve(mockResponse)
    } as any);

    const classifier = new TopicClassifier(classifierArn);
    const result = await classifier.classifyText(sampleText);

    deepStrictEqual(result, ['Topic1', 'Topic2']);
  });

  it('propagates AWS errors', async () => {
    const error = new Error('AWS Error');
    td.when(comprehendClient.classifyDocument({
      Text: sampleText,
      EndpointArn: classifierArn
    })).thenReturn({
      promise: () => Promise.reject(error)
    } as any);

    const classifier = new TopicClassifier(classifierArn);

    try {
      await classifier.classifyText(sampleText);
      throw new Error('Should have thrown an error');
    } catch (e) {
      deepStrictEqual(e, error);
    }
  });
});
