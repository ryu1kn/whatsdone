import _pick = require('lodash.pick');
import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';
import {DoneDiff} from '../models/Done';

export default class UpdateDoneCommand {
  private _doneRepository: DoneRepository;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(data: DoneDiff, doneId: string, userId: string) {
    const done = await this._doneRepository.update(doneId, userId, data);
    return {..._pick(done, ['id', 'userId', 'date', 'doneThing']), topics: ["foo"]};
  }

}
