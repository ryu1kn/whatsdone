
const ServiceLocator = require('../ServiceLocator');

class CreateDoneCommand {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  execute(params) {
    const writeParams = Object.assign({}, params.data, {userId: params.userId});
    return this._doneRepository.write(writeParams)
      .then(done => Object.assign({}, done, {username: params.username}));
  }

}

module.exports = CreateDoneCommand;
