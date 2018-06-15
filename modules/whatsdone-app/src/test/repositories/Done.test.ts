
import ServiceLocator from '../../lib/ServiceLocator';
import DoneRepository from '../../lib/repositories/Done';
import {expect} from '../TestUtils';
import sinon = require('sinon');
import ServiceFactory from '../../lib/ServiceFactory';

describe('Server DoneRepository', () => {

  it('reads done items from DB', () => {
    const doneQueryHelper = {
      query: () => Promise.resolve('QUERY_RESULT')
    };
    initialiseServiceLocator(doneQueryHelper);
    const repository = new DoneRepository();

    return repository.read().then(result => {
      expect(result).to.eql('QUERY_RESULT');
    });
  });

  it('record a new done item', () => {
    const doneDynamoTableClient = {
      put: sinon.stub().returns(Promise.resolve('DONE_ID')),
      getById: () => {}
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    const done = {
      date: '2017-08-14T12:26:26.227Z',
      doneThing: 'DONE_THING'
    };
    return repository.write(done).then(() => {
      expect(doneDynamoTableClient.put.args[0][0]).to.includes({
        date: '2017-08-14T12:26:26.227Z',
        doneThing: 'DONE_THING'
      });
    });
  });

  it('gets the newly created done item back', () => {
    const doneDynamoTableClient = {
      put: () => Promise.resolve('DONE_ID'),
      getById: sinon.stub().returns(Promise.resolve({DATA: '..'}))
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();
    const done = {
      date: '2017-08-14T12:26:26.227Z',
      doneThing: 'DONE_THING'
    };
    return repository.write(done).then(newDone => {
      expect(newDone).to.eql({DATA: '..'});
      expect(doneDynamoTableClient.getById).to.have.been.calledWith('DONE_ID');
    });
  });

  it('adds "month" fields when it saves a done', () => {
    const doneDynamoTableClient = {
      put: sinon.stub().returns(Promise.resolve('DONE_ID')),
      getById: () => Promise.resolve()
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    const done = {
      date: '2017-08-14T12:26:26.227Z',
      doneThing: 'DONE_THING'
    };
    return repository.write(done).then(() => {
      expect(doneDynamoTableClient.put).to.have.been.calledWith({
        date: '2017-08-14T12:26:26.227Z',
        doneThing: 'DONE_THING',
        month: '2017-08'
      });
    });
  });

  it('does not return "month" field when returning created done', () => {
    const doneDynamoTableClient = {
      put: () => Promise.resolve('DONE_ID'),
      getById: sinon.stub().returns(Promise.resolve({
        DATA: '..',
        month: 'MONTH'
      }))
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();
    const done = {
      date: '2017-08-14T12:26:26.227Z',
      doneThing: 'DONE_THING'
    };
    return repository.write(done).then(newDone => {
      expect(newDone).to.eql({DATA: '..'});
    });
  });

  it('remove a done if the requesting user is the owner', () => {
    const matchingDone = {userId: 'USER_ID', DATA: '..'};
    const doneDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve(matchingDone)),
      delete: sinon.spy()
    };
    initialiseServiceLocator({}, doneDynamoTableClient);
    const repository = new DoneRepository();

    return repository.remove('DONE_ID', 'USER_ID').then(() => {
      expect(doneDynamoTableClient.getById).to.have.been.calledWith('DONE_ID');
      expect(doneDynamoTableClient.delete).to.have.been.calledWith('DONE_ID');
    });
  });

  it('updates a done if the requesting user is the owner', () => {
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
    return repository.update('DONE_ID', 'USER_ID', newData).then(() => {
      expect(doneDynamoTableClient.getById).to.have.been.calledWith('DONE_ID');
      const updateArgs = doneDynamoTableClient.update.args[0];
      expect(updateArgs[0]).to.eql('DONE_ID');
      expect(updateArgs[1]).to.include({
        date: '2017-08-14T12:26:26.227Z',
        doneThing: 'NEW_DONE_THING'
      });
    });
  });

  it('updates "month" field if date is going to be updated', () => {
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
    return repository.update('DONE_ID', 'USER_ID', newData).then(() => {
      const newDone = doneDynamoTableClient.update.args[0][1];
      expect(newDone).to.include({
        date: '2017-08-14T12:26:26.227Z',
        month: '2017-08'
      });
    });
  });

  it('does not include "month" in the update response', () => {
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
    return repository.update('DONE_ID', 'USER_ID', newData).then(done => {
      expect(done).to.eql({DATA: '..'});
    });
  });

  let initialiseServiceLocator = function (doneQueryHelper: {query: any} | {}, doneDynamoTableClient?: {getById: any, update?: any, delete?: any, put?: any}) {
    ServiceLocator.load({
      createDoneDynamoTableClient: () => doneDynamoTableClient,
      createDoneQueryHelper: () => doneQueryHelper
    } as ServiceFactory);
  };
});
