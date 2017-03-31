
'use strict';

const ServiceLocator = require('../ServiceLocator');

class CreateDoneCommand {

  constructor() {
    this._userRepository = ServiceLocator.userRepository;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  execute(params) {
    const writeParams = Object.assign({}, params.data, {userId: params.userId});
    return this._doneRepository.write(writeParams)
      .then(done => this._setUserName(done));
  }

  _setUserName(done) {
    const userPromise = done.userId ? this._userRepository.getById(done.userId) : Promise.resolve({name: null});
    return userPromise.then(user => Object.assign(done, {username: user.name}));
  }

}

module.exports = CreateDoneCommand;
