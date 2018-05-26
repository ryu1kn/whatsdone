
const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class UpdateDoneCommand {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(params) {
    const done = await this._doneRepository.update(params.doneId, params.userId, params.data);
    return _.pick(done, ['id', 'userId', 'date', 'doneThing']);
  }

}

module.exports = UpdateDoneCommand;
