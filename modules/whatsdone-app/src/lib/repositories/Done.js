
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');
const utils = require('./utils');

const MODIFIABLE_FIELDS = ['date', 'doneThing'];

class DoneRepository {

  constructor() {
    this._doneDynamoTableClient = ServiceLocator.doneDynamoTableClient;
    this._doneQueryHelper = ServiceLocator.doneQueryHelper;
  }

  read(nextKey) {
    return this._doneQueryHelper.query(nextKey);
  }

  write(done) {
    const finalDone = utils.getDoneWithMonth(done);
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
        const finalOverwrite = utils.getDoneWithMonth(doneOverwrite);
        return this._doneDynamoTableClient.update(id, finalOverwrite)
          .then(done => _.omit(done, 'month'));
      });
  }

}

module.exports = DoneRepository;
