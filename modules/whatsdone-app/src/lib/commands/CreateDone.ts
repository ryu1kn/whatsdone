import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';
import {Done, DoneInDb} from '../models/Done';
import TopicClassifier from '../services/TopicClassifier';

export default class CreateDoneCommand {
  private _doneRepository: DoneRepository;
  private _topicClassifier: TopicClassifier;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
    this._topicClassifier = ServiceLocator.topicClassifier;
  }

  async execute(data: Done, userId: string) {
    try {
      const topics = await this._topicClassifier.classifyText(data.doneThing);
      return this._doneRepository.write({...data, userId, topics});
    } catch (error) {
      console.error(error);
      return this._doneRepository.write({...data, userId, topics: []});
    }
  }
}
