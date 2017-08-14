
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

const MODIFIABLE_FIELDS = ['date', 'doneThing'];
const MONTH_LENGTH = 'YYYY-MM'.length;

class DoneRepository {

  constructor() {
    this._doneDynamoTableClient = ServiceLocator.doneDynamoTableClient;
  }

  read() {
    return this._doneDynamoTableClient.getAll()
      .then(dones => dones.map(done => _.omit(done, 'month')));
  }

  write(done) {
    const finalDone = this._getDoneWithMonth(done);
    return this._doneDynamoTableClient.put(finalDone)
      .then(id => this._doneDynamoTableClient.getById(id))
      .then(doneWithId => _.omit(doneWithId, 'month'));
  }

  remove(id, currentUserId) {
    return this._doneDynamoTableClient.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDenied]: You don\'t have the permission to delete this item.');
        }
        return this._doneDynamoTableClient.delete(id);
      });
  }

  update(id, currentUserId, newData) {
    return this._doneDynamoTableClient.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDenied]: You don\'t have the permission to modify this item.');
        }
        const doneOverwrite = _.pick(newData, MODIFIABLE_FIELDS);
        const finalOverwrite = this._getDoneWithMonth(doneOverwrite);
        return this._doneDynamoTableClient.update(id, finalOverwrite)
          .then(done => _.omit(done, 'month'));
      });
  }

  _getDoneWithMonth(done) {
    if (!done.date) return done;
    return Object.assign({}, done, {
      month: done.date.substr(0, MONTH_LENGTH)
    });
  }

}

module.exports = DoneRepository;
