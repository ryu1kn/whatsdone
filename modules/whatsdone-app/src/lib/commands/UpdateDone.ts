import _ = require('lodash');
import ServiceLocator from '../ServiceLocator';

export default class UpdateDoneCommand {
  private _doneRepository: any;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(params) {
    const done = await this._doneRepository.update(params.doneId, params.userId, params.data);
    return _.pick(done, ['id', 'userId', 'date', 'doneThing']);
  }

}
