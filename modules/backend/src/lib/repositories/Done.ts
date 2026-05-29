import _omit = require('lodash.omit');
import ServiceLocator from '../ServiceLocator';
import * as utils from './utils';
import DynamoTableClient from './DynamoTableClient';
import DoneQueryHelper, {DoneQueryResult} from './done-helpers/query';
import {DoneDiff, DoneInDb, UserDone} from '../models/Done';

const MODIFIABLE_FIELDS: (keyof DoneDiff)[] = ['date', 'doneThing'];

// Keep only the whitelisted fields that are actually present on the input,
// mirroring lodash.pick semantics (absent keys are not added as `undefined`).
const pickModifiableFields = (newData: DoneDiff): DoneDiff =>
  MODIFIABLE_FIELDS.reduce((overwrite: DoneDiff, field) => {
    if (field in newData) {
      overwrite[field] = newData[field];
    }
    return overwrite;
  }, {});

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
    const doneWithId = await this._doneDynamoTableClient.getById(id) as DoneInDb;
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
    const doneOverwrite = pickModifiableFields(newData);
    const finalOverwrite = utils.getDoneWithMonth(doneOverwrite);
    const done = await this._doneDynamoTableClient.update(id, finalOverwrite) as DoneInDb;
    return _omit(done, 'month');
  }

}
