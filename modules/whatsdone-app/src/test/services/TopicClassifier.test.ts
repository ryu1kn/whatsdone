import ServiceLocator from '../../lib/ServiceLocator';
import TopicClassifier from '../../lib/services/TopicClassifier';
import {deepStrictEqual} from 'assert';
import * as td from 'testdouble';
import AWS = require('aws-sdk');
import ServiceFactory from '../../lib/ServiceFactory';
import { Logger } from '../../lib/Logger';

describe('TopicClassifier', () => {
  const classifierArn = 'test-classifier-arn';
  const sampleText = 'Sample text to classify';

  let comprehendClient: AWS.Comprehend;
  let translateClient: AWS.Translate;
  let logger: Logger;

  beforeEach(() => {
    // Create a mock Comprehend client with the required methods
    comprehendClient = {
      classifyDocument: () => ({}), // This will be overridden by td.when
      detectDominantLanguage: () => ({}) // This will be overridden by td.when
    } as any;
    td.replace(comprehendClient, 'classifyDocument');
    td.replace(comprehendClient, 'detectDominantLanguage');

    // Create a mock Translate client
    translateClient = {
      translateText: () => ({}) // This will be overridden by td.when
    } as any;
    td.replace(translateClient, 'translateText');

    // Create a logger instance and mock its methods
    logger = new Logger('DEBUG');
    td.replace(logger, 'debug');
    td.replace(logger, 'error');
    td.replace(logger, 'warn');
    td.replace(logger, 'info');

    const factory = {
      createComprehendClient: () => comprehendClient,
      createTranslateClient: () => translateClient,
      createLogger: () => logger
    } as Partial<ServiceFactory> as ServiceFactory;

    ServiceLocator.load(factory);
  });

  afterEach(() => {
    td.reset();
  });

  it('classifies text and returns topics', async () => {
    // Mock language detection response for English
    td.when(comprehendClient.detectDominantLanguage({
      Text: sampleText
    })).thenReturn({
      promise: () => Promise.resolve({
        Languages: [
          { LanguageCode: 'en', Score: 0.99 }
        ]
      })
    } as any);

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

    // Mock language detection to fail
    td.when(comprehendClient.detectDominantLanguage({
      Text: sampleText
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

  it('classifies Japanese text by translating it first', async () => {
    const japaneseText = '今日はコードをたくさん書きました';
    const translatedText = 'I wrote a lot of code today';

    // Mock language detection response
    td.when(comprehendClient.detectDominantLanguage({
      Text: japaneseText
    })).thenReturn({
      promise: () => Promise.resolve({
        Languages: [
          { LanguageCode: 'ja', Score: 0.99 }
        ]
      })
    } as any);

    // Mock translation response
    td.when(translateClient.translateText({
      Text: japaneseText,
      SourceLanguageCode: 'ja',
      TargetLanguageCode: 'en'
    })).thenReturn({
      promise: () => Promise.resolve({
        TranslatedText: translatedText,
        SourceLanguageCode: 'ja',
        TargetLanguageCode: 'en'
      })
    } as any);

    // Mock classification response for the translated text
    const mockResponse = {
      Classes: [
        { Name: 'Development', Score: 0.9 },
        { Name: 'Coding', Score: 0.8 }
      ]
    } as AWS.Comprehend.ClassifyDocumentResponse;

    td.when(comprehendClient.classifyDocument({
      Text: translatedText,
      EndpointArn: classifierArn
    })).thenReturn({
      promise: () => Promise.resolve(mockResponse)
    } as any);

    const classifier = new TopicClassifier(classifierArn);
    const result = await classifier.classifyText(japaneseText);

    deepStrictEqual(result, ['Development', 'Coding']);
  });
});
