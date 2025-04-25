import _map = require('lodash.map');
import _keyBy = require('lodash.keyby');
import _get = require('lodash.get');
import ServiceLocator from '../ServiceLocator';
import UserNameService from '../UserNameService';
import DoneRepository from '../repositories/Done';
import {DoneInDb} from '../models/Done';
import TopicClassifier from '../services/TopicClassifier';

export default class GetDonesCommand {
  private _userNameService: UserNameService;
  private _doneRepository: DoneRepository;
  private _topicClassifier: TopicClassifier;

  constructor() {
    this._userNameService = ServiceLocator.userNameService;
    this._doneRepository = ServiceLocator.doneRepository;
    this._topicClassifier = ServiceLocator.topicClassifier;
  }

  async execute(nextKey?: string) {
    const result = await this._doneRepository.read(nextKey);
    const itemsWithUsernames = await this.setUserNames(result.items);
    const itemsWithTopics = await this.setTopics(itemsWithUsernames);
    return {
      items: itemsWithTopics,
      nextKey: result.nextKey
    };
  }

  private async setUserNames(dones: DoneInDb[]) {
    const users = await this._userNameService.getUsernames(_map(dones, 'userId'));
    const nameMap = _keyBy(users, 'id');
    return dones.map(done =>
      done.userId ?
        Object.assign({}, done, {username: _get(nameMap, `${done.userId}.name`)}) :
        done
    );
  }

  private async setTopics(dones: (DoneInDb & {username?: string})[]) {
    const donesWithTopics = await Promise.all(dones.map(async done => {
      const topics = await this._topicClassifier.classifyText(done.doneThing);
      return {...done, topics};
    }));
    return donesWithTopics;
  }
}
