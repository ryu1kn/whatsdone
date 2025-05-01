import AWS = require('aws-sdk');
import ServiceLocator from '../ServiceLocator';
import { Logger } from '../Logger';

export default class TopicClassifier {
  private _comprehend: AWS.Comprehend;
  private _translate: AWS.Translate;
  private _classifierArn: string;
  private _logger: Logger;

  constructor(classifier_arn: string) {
    this._comprehend = ServiceLocator.comprehendClient;
    this._translate = ServiceLocator.translateClient;
    this._classifierArn = classifier_arn;
    this._logger = ServiceLocator.logger;
  }

  async classifyText(text: string): Promise<string[]> {
    const dominantLanguage = await this.detectLanguage(text);

    this._logger.debug(`Dominant language: ${dominantLanguage}`);

    const textToClassify = await this.ensureEnglish(text, dominantLanguage);

    const topicsWithConfidence = await this._classifyText(textToClassify);

    this._logger.debug(`Topics with confidence: ${JSON.stringify(topicsWithConfidence)}`);

    return topicsWithConfidence.map(c => c.topic);
  }

  private async detectLanguage(text: string): Promise<string | undefined> {
    const languageResult = await this._comprehend.detectDominantLanguage({
      Text: text
    }).promise();

    return languageResult.Languages?.[0]?.LanguageCode;
  }

  private async ensureEnglish(text: string, sourceLanguage: string | undefined): Promise<string> {
    if (!sourceLanguage || sourceLanguage === 'en') return text;

    const translateResult = await this._translate.translateText({
      Text: text,
      SourceLanguageCode: sourceLanguage,
      TargetLanguageCode: 'en'
    }).promise();

    return translateResult.TranslatedText;
  }

  private async _classifyText(text: string): Promise<{topic: string, confidence: number}[]> {
    const result = await this._comprehend.classifyDocument({
      Text: text,
      EndpointArn: this._classifierArn
    }).promise();

    return (result.Classes ?? []).map(c => ({topic: c.Name!, confidence: c.Score!}));
  }
}
