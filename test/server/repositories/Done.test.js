
const ServiceLocator = require('../../../src/server/ServiceLocator');
const DoneRepository = require('../../../src/server/repositories/Done');

describe('Server DoneRepository', () => {

  it('reads all done items from DB', () => {
    const doneDynamoTableClient = {
      getAll: () => Promise.resolve('DONES')
    };
    ServiceLocator.load({
      createDoneDynamoTableClient: () => doneDynamoTableClient
    });
    const repository = new DoneRepository();

    return repository.read().then(dones => {
      expect(dones).to.eql('DONES');
    });
  });

  it('record a new done item', () => {
    const doneDynamoTableClient = {
      put: sinon.stub().returns(Promise.resolve('DONE_ID')),
      getById: sinon.stub().returns(Promise.resolve('NEW_DONE_WITH_ID'))
    };
    ServiceLocator.load({
      createDoneDynamoTableClient: () => doneDynamoTableClient
    });
    const repository = new DoneRepository();

    return repository.write('NEW_DONE').then(newDone => {
      expect(newDone).to.eql('NEW_DONE_WITH_ID');
      expect(doneDynamoTableClient.put).to.have.been.calledWith('NEW_DONE');
      expect(doneDynamoTableClient.getById).to.have.been.calledWith('DONE_ID');
    });
  });

  it('remove a done if the requesting user is the owner', () => {
    const matchingDone = {userId: 'USER_ID', DATA: '..'};
    const doneDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve(matchingDone)),
      delete: sinon.spy()
    };
    ServiceLocator.load({
      createDoneDynamoTableClient: () => doneDynamoTableClient
    });
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
      update: sinon.stub().returns(Promise.resolve('UPDATED_DINE'))
    };
    ServiceLocator.load({
      createDoneDynamoTableClient: () => doneDynamoTableClient
    });
    const repository = new DoneRepository();

    const newData = {
      date: 'NEW_DATE',
      doneThing: 'NEW_DONE_THING',
      NON_UPDATABLE_KEY: 'NEW ..'
    };
    return repository.update('DONE_ID', 'USER_ID', newData).then(done => {
      expect(done).to.eql('UPDATED_DINE');
      expect(doneDynamoTableClient.getById).to.have.been.calledWith('DONE_ID');
      expect(doneDynamoTableClient.update).to.have.been.calledWith(
        'DONE_ID', {date: 'NEW_DATE', doneThing: 'NEW_DONE_THING'}
      );
    });
  });

});
