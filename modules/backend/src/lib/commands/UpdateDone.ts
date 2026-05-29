import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';
import {DoneDiff} from '../models/Done';

export default class UpdateDoneCommand {
  private _doneRepository: DoneRepository;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(data: DoneDiff, doneId: string, currentUserId: string) {
    const done = await this._doneRepository.update(doneId, currentUserId, data);
    const {id, userId, date, doneThing} = done;
    return {id, userId, date, doneThing, topics: ['foo']};
  }

}
