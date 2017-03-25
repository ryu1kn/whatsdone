
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class LoginCommand {

  constructor() {
    this._userRepository = ServiceLocator.userRepository;
    this._sessionRepository = ServiceLocator.sessionRepository;
  }

  execute(params) {
    const findParams = _.pick(params, ['email', 'password']);
    return this._userRepository.findUser(findParams).then(user => {
      if (!user) return null;

      const params = {
        userId: user.id,
        isAuthorized: true
      };
      return this._sessionRepository.write(params)
        .then(session => session.id);
    });
  }

}

module.exports = LoginCommand;
