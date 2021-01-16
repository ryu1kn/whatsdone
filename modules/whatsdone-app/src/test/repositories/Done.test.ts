import ServiceLocator from '../../lib/ServiceLocator';
import DoneRepository from '../../lib/repositories/Done';
import ServiceFactory from '../../lib/ServiceFactory';
import {deepStrictEqual} from 'assert';
import * as td from 'testdouble';
import DynamoTableClient from '../../lib/repositories/DynamoTableClient';
import _omit = require('lodash.omit');

describe('Server DoneRepository', () => {
  const doneId = 'DONE_ID';
  const userId = 'USER_ID';
  const doneItem = {
    date: '2017-08-14T12:26:26.227Z',
    doneThing: 'DONE_THING',
    userId
  };
  const doneItemWithMonth = {...doneItem, month: '2017-08'};
  const doneItemSaved = {...doneItemWithMonth, doneId};

  it('reads done items from DB', async () => {
    const doneQueryHelper = {
      query: () => Promise.resolve('QUERY_RESULT')
    };
    initialiseServiceLocator(doneQueryHelper);
    const repository = new DoneRepository();

    const result = await repository.read();
    deepStrictEqual(result, 'QUERY_RESULT');
  });

  it('records a new done with its month and returns it except "month"', async () => {
    const dynamoTableClient = td.instance(DynamoTableClient);
    td.when(dynamoTableClient.put(doneItemWithMonth)).thenResolve(doneId);
    td.when(dynamoTableClient.getById(doneId)).thenResolve(doneItemSaved);

    initialiseServiceLocator({}, dynamoTableClient);
    const repository = new DoneRepository();

    const newDone = await repository.write(doneItem);

    deepStrictEqual(newDone, _omit(doneItemSaved, 'month'));
  });

  it('remove a done if the requesting user is the owner', async () => {
    const dynamoTableClient = td.instance(DynamoTableClient);
    td.when(dynamoTableClient.getById(doneId)).thenResolve(doneItemSaved);

    initialiseServiceLocator({}, dynamoTableClient);
    const repository = new DoneRepository();

    await repository.remove(doneId, userId);

    td.verify(dynamoTableClient.delete(doneId));
  });

  it('updates a done if the requesting user is the owner', async () => {
    const dynamoTableClient = td.instance(DynamoTableClient);
    td.when(dynamoTableClient.getById(doneId)).thenResolve({...doneItemSaved, NON_UPDATABLE_KEY: '..'});

    initialiseServiceLocator({}, dynamoTableClient);
    const repository = new DoneRepository();

    const newData = {
      ...doneItem,
      doneThing: 'NEW_DONE_THING',
      NON_UPDATABLE_KEY: 'NEW ..'
    };
    await repository.update('DONE_ID', 'USER_ID', newData);

    td.verify(dynamoTableClient.update(
      doneId,
      _omit({...doneItemWithMonth, doneThing: 'NEW_DONE_THING'}, 'userId')
    ));
  });

  // TODO: Revise the test data
  it('updates "month" field if date is going to be updated', async () => {
    const matchingDone = {
      userId: 'USER_ID',
      date: 'DATE',
      doneThing: 'DONE_THING'
    };
    const dynamoTableClient = td.instance(DynamoTableClient);
    td.when(dynamoTableClient.getById(doneId)).thenResolve(matchingDone);
    td.when(dynamoTableClient.update(doneId, {
      date: '2017-08-14T12:26:26.227Z',
      month: '2017-08',
      doneThing: 'NEW_DONE_THING'
    })).thenResolve({...doneItemWithMonth, doneThing: 'NEW_DONE_THING'});

    initialiseServiceLocator({}, dynamoTableClient);
    const repository = new DoneRepository();

    const newData = {
      date: '2017-08-14T12:26:26.227Z',
      doneThing: 'NEW_DONE_THING'
    };
    const result = await repository.update('DONE_ID', 'USER_ID', newData);

    deepStrictEqual(result, {...doneItem, doneThing: 'NEW_DONE_THING'});
  });

  let initialiseServiceLocator = function (doneQueryHelper: {query: any} | {}, dynamoTableClient?: DynamoTableClient) {
    ServiceLocator.load({
      createDoneDynamoTableClient: () => dynamoTableClient,
      createDoneQueryHelper: () => doneQueryHelper
    } as ServiceFactory);
  };
});
