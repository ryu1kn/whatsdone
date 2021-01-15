import ServiceLocator from '../../lib/ServiceLocator';
import DoneRepository from '../../lib/repositories/Done';
import ServiceFactory from '../../lib/ServiceFactory';
import {deepStrictEqual} from 'assert';
import * as td from 'testdouble';
import sinon = require('sinon');

describe('Server DoneRepository', () => {
  const doneItem = {
    date: '2017-08-14T12:26:26.227Z',
    doneThing: 'DONE_THING'
  };
  const doneItemWithMonth = {...doneItem, month: '2017-08'};

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
    const doneDynamoTableClient = {
      put: td.when(td.func()(doneItemWithMonth)).thenResolve('DONE_ID'),
      getById: td.when(td.func()('DONE_ID')).thenResolve(doneItemWithMonth)
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    const newDone = await repository.write(doneItem);

    deepStrictEqual(newDone, doneItem);
  });

  it('remove a done if the requesting user is the owner', async () => {
    const matchingDone = {userId: 'USER_ID', DATA: '..'};
    const doneDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve(matchingDone)),
      delete: sinon.spy()
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    await repository.remove('DONE_ID', 'USER_ID');
    deepStrictEqual(doneDynamoTableClient.getById.args[0], ['DONE_ID']);
    deepStrictEqual(doneDynamoTableClient.delete.args[0], ['DONE_ID']);
  });

  it('updates a done if the requesting user is the owner', async () => {
    const matchingDone = {
      userId: 'USER_ID',
      date: 'DATE',
      doneThing: 'DONE_THING',
      NON_UPDATABLE_KEY: '..'
    };
    const doneDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve(matchingDone)),
      update: sinon.stub().returns(Promise.resolve())
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    const newData = {
      date: '2017-08-14T12:26:26.227Z',
      doneThing: 'NEW_DONE_THING',
      NON_UPDATABLE_KEY: 'NEW ..'
    };
    await repository.update('DONE_ID', 'USER_ID', newData);
    deepStrictEqual(doneDynamoTableClient.getById.args[0], ['DONE_ID']);
    const updateArgs = doneDynamoTableClient.update.args[0];
    deepStrictEqual(updateArgs[0], 'DONE_ID');
    deepStrictEqual(updateArgs[1], {
      date: '2017-08-14T12:26:26.227Z',
      month: '2017-08',
      doneThing: 'NEW_DONE_THING'
    });
  });

  it('updates "month" field if date is going to be updated', async () => {
    const matchingDone = {
      userId: 'USER_ID',
      date: 'DATE',
      doneThing: 'DONE_THING'
    };
    const doneDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve(matchingDone)),
      update: sinon.stub().returns(Promise.resolve())
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    const newData = {
      date: '2017-08-14T12:26:26.227Z',
      doneThing: 'NEW_DONE_THING'
    };
    await repository.update('DONE_ID', 'USER_ID', newData);
    const newDone = doneDynamoTableClient.update.args[0][1];
    deepStrictEqual(newDone, {
      date: '2017-08-14T12:26:26.227Z',
      month: '2017-08',
      doneThing: 'NEW_DONE_THING'
    });
  });

  it('does not include "month" in the update response', async () => {
    const matchingDone = {
      userId: 'USER_ID',
      date: 'DATE',
      doneThing: 'DONE_THING'
    };
    const doneDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve(matchingDone)),
      update: sinon.stub().returns(Promise.resolve({
        DATA: '..',
        month: 'MONTH'
      }))
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    const newData = {doneThing: 'NEW_DONE_THING'};
    const done = await repository.update('DONE_ID', 'USER_ID', newData);
    deepStrictEqual(done, {DATA: '..'});
  });

  let initialiseServiceLocator = function (doneQueryHelper: {query: any} | {}, doneDynamoTableClient?: {getById: any, update?: any, delete?: any, put?: any}) {
    ServiceLocator.load({
      createDoneDynamoTableClient: () => doneDynamoTableClient,
      createDoneQueryHelper: () => doneQueryHelper
    } as ServiceFactory);
  };
});
