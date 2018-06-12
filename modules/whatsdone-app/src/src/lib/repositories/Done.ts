
import _ = require('lodash');
import ServiceLocator = require('../ServiceLocator');
import utils = require('./utils');

const MODIFIABLE_FIELDS = ['date', 'doneThing'];

class DoneRepository {
  private _doneDynamoTableClient: any;
  private _doneQueryHelper: any;

  constructor() {
    this._doneDynamoTableClient = ServiceLocator.doneDynamoTableClient;
    this._doneQueryHelper = ServiceLocator.doneQueryHelper;
  }

  read(nextKey?) {
    return this._doneQueryHelper.query(nextKey);
  }

  async write(done) {
    const finalDone = utils.getDoneWithMonth(done);
    const id = await this._doneDynamoTableClient.put(finalDone);
    const doneWithId = await this._doneDynamoTableClient.getById(id);
    return _.omit(doneWithId, 'month');
  }

  async remove(id, currentUserId) {
    const found = await this._doneDynamoTableClient.getById(id);
    if (found === null) {
      throw new Error('[NotFound]: Done item not found');
    }
    if (found.userId !== currentUserId) {
      throw new Error('[AccessDenied]: You don\'t have the permission to delete this item.');
    }
    return this._doneDynamoTableClient.delete(id);
  }

  async update(id, currentUserId, newData) {
    const found = await this._doneDynamoTableClient.getById(id);
    if (found === null) {
      throw new Error('[NotFound]: Done item not found');
    }
    if (found.userId !== currentUserId) {
      throw new Error('[AccessDenied]: You don\'t have the permission to modify this item.');
    }
    const doneOverwrite = _.pick(newData, MODIFIABLE_FIELDS);
    const finalOverwrite = utils.getDoneWithMonth(doneOverwrite);
    const done = await this._doneDynamoTableClient.update(id, finalOverwrite);
    return _.omit(done, 'month');
  }

}

export = DoneRepository;
