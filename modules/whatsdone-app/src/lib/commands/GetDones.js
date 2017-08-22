
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class GetDonesCommand {

  constructor() {
    this._userRepository = ServiceLocator.userRepository;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  execute() {
    return this._doneRepository.read()
      .then(result => {
        return this._setUserNames(result.items)
          .then(items => ({
            items,
            nextKey: result.nextKey
          }));
      });
  }

  _setUserNames(dones) {
    return this._userRepository.getByIds(_.map(dones, 'userId'))
      .then(users => {
        const nameMap = _.keyBy(users, 'id');
        return dones.map(done => {
          if (done.userId) {
            done.username = _.get(nameMap, `${done.userId}.name`);
          }
          return done;
        });
      });
  }

}

module.exports = GetDonesCommand;
