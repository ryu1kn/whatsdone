import _pick = require('lodash.pick');
import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';

export default class UpdateDoneCommand {
  private _doneRepository: DoneRepository;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(params) {
    const done = await this._doneRepository.update(params.doneId, params.userId, params.data);
    return _pick(done, ['id', 'userId', 'date', 'doneThing']);
  }

}
