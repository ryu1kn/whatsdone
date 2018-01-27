
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class GetDonesCommand {

  constructor() {
    this._userNameService = ServiceLocator.userNameService;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  execute(nextKey) {
    return this._doneRepository.read(nextKey)
      .then(result => {
        return this._setUserNames(result.items)
          .then(items => ({
            items,
            nextKey: result.nextKey
          }));
      });
  }

  _setUserNames(dones) {
    return this._userNameService.getUsernames(_.map(dones, 'userId'))
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
