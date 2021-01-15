import _pick = require('lodash.pick');
import _omit = require('lodash.omit');
import ServiceLocator from '../ServiceLocator';
import * as utils from './utils';
import DynamoTableClient from './DynamoTableClient';
import DoneQueryHelper, {DoneQueryResult} from './done-helpers/query';
import {DoneDiff, DoneInDb, UserDone} from '../models/Done';

const MODIFIABLE_FIELDS = ['date', 'doneThing'];

export default class DoneRepository {
  private _doneDynamoTableClient: DynamoTableClient;
  private _doneQueryHelper: DoneQueryHelper;

  constructor() {
    this._doneDynamoTableClient = ServiceLocator.doneDynamoTableClient;
    this._doneQueryHelper = ServiceLocator.doneQueryHelper;
  }

  read(nextKey?: string): Promise<DoneQueryResult> {
    return this._doneQueryHelper.query(nextKey);
  }

  async write(done: UserDone): Promise<Omit<DoneInDb, 'month'>> {
    const finalDone = utils.getDoneWithMonth(done);
    const id = await this._doneDynamoTableClient.put(finalDone);
    const doneWithId = await this._doneDynamoTableClient.getById(id);
    return _omit(doneWithId, 'month');
  }

  async remove(id: string, currentUserId: string): Promise<void> {
    const found = await this._doneDynamoTableClient.getById(id);
    if (found === null) {
      throw new Error('[NotFound]: Done item not found');
    }
    if (found.userId !== currentUserId) {
      throw new Error('[AccessDenied]: You don\'t have the permission to delete this item.');
    }
    return this._doneDynamoTableClient.delete(id);
  }

  async update(id: string, currentUserId: string, newData: DoneDiff): Promise<Omit<DoneInDb, 'month'>> {
    const found = await this._doneDynamoTableClient.getById(id);
    if (found === null) {
      throw new Error('[NotFound]: Done item not found');
    }
    if (found.userId !== currentUserId) {
      throw new Error('[AccessDenied]: You don\'t have the permission to modify this item.');
    }
    const doneOverwrite = _pick(newData, MODIFIABLE_FIELDS);
    const finalOverwrite = utils.getDoneWithMonth(doneOverwrite);
    const done = await this._doneDynamoTableClient.update(id, finalOverwrite);
    return _omit(done, 'month');
  }

}
