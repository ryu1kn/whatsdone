import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';
import {Done} from '../models/Done';
import TopicClassifier from '../services/TopicClassifier';
import { Logger } from '../Logger';

export default class CreateDoneCommand {
  private _doneRepository: DoneRepository;
  private _topicClassifier: TopicClassifier;
  private _logger: Logger;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
    this._topicClassifier = ServiceLocator.topicClassifier;
    this._logger = ServiceLocator.logger;
  }

  async execute(data: Done, userId: string) {
    const topics = await this.classifyText(data.doneThing);
    return this._doneRepository.write({...data, userId, topics});
  }

  private async classifyText(text: string): Promise<string[]> {
    try {
      return await this._topicClassifier.classifyText(text);
    } catch (error) {
      this._logger.error('Failed to classify text:', error);
      return [];
    }
  }
}
