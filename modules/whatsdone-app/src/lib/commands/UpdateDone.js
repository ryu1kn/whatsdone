
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class UpdateDoneCommand {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  execute(params) {
    return this._doneRepository.update(params.doneId, params.userId, params.data)
      .then(done => _.pick(done, ['id', 'userId', 'date', 'doneThing']));
  }

}

module.exports = UpdateDoneCommand;
