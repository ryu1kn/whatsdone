
const ServiceLocator = require('../ServiceLocator');

class CreateDoneCommand {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(params) {
    const writeParams = Object.assign({}, params.data, {userId: params.userId});
    const done = await this._doneRepository.write(writeParams);
    return Object.assign({}, done, {username: params.username});
  }

}

module.exports = CreateDoneCommand;
