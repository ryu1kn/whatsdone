
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class GetDonesRequestHandler {

  constructor() {
    this._userRepository = ServiceLocator.userRepository;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  handle(req, res, next) {
    this._doneRepository.read()
      .then(dones => this._setUserNames(dones))
      .then(dones => {
        res.setHeader('Content-Type', 'application/json');
        res.send(dones);
      })
      .catch(next);
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

module.exports = GetDonesRequestHandler;
