import _map = require('lodash.map');
import _keyBy = require('lodash.keyby');
import _get = require('lodash.get');
import ServiceLocator from '../ServiceLocator';
import UserNameService from '../UserNameService';
import DoneRepository from '../repositories/Done';
import {DoneInDb} from '../models/Done';

export default class GetDonesCommand {
  private _userNameService: UserNameService;
  private _doneRepository: DoneRepository;

  constructor() {
    this._userNameService = ServiceLocator.userNameService;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(nextKey?: string) {
    const result = await this._doneRepository.read(nextKey);
    const itemsWithTopics = this.ensureTopics(result.items);
    const itemsWithUsernames = await this.setUserNames(itemsWithTopics);
    return {
      items: itemsWithUsernames,
      nextKey: result.nextKey
    };
  }

  private ensureTopics(dones: DoneInDb[]) {
    return dones.map(d => ({...d, topics: d.topics ?? []}))
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
}
