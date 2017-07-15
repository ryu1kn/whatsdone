
const ServiceLocator = require('../../lib/ServiceLocator');
const SessionRepository = require('../../lib/repositories/Session');

describe('Server SessionRepository', () => {

  it('record a new session', () => {
    const sessionDynamoTableClient = {
      put: sinon.stub().returns(Promise.resolve('SESSION_ID'))
    };
    ServiceLocator.load({
      createSessionDynamoTableClient: () => sessionDynamoTableClient
    });
    const repository = new SessionRepository();

    return repository.write('SESSION_INFO').then(sessionId => {
      expect(sessionId).to.eql('SESSION_ID');
      expect(sessionDynamoTableClient.put).to.have.been.calledWith('SESSION_INFO');
    });
  });

  it('gets session by id', () => {
    const sessionDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve('SESSION'))
    };
    ServiceLocator.load({
      createSessionDynamoTableClient: () => sessionDynamoTableClient
    });
    const repository = new SessionRepository();

    return repository.getById('SESSION_ID').then(user => {
      expect(user).to.eql('SESSION');
      expect(sessionDynamoTableClient.getById).to.have.been.calledWith('SESSION_ID');
    });
  });

  it('removes a session', () => {
    const sessionDynamoTableClient = {delete: sinon.stub().returns(Promise.resolve())};
    ServiceLocator.load({
      createSessionDynamoTableClient: () => sessionDynamoTableClient
    });
    const repository = new SessionRepository();

    return repository.remove('SESSION_ID').then(() => {
      expect(sessionDynamoTableClient.delete).to.have.been.calledWith('SESSION_ID');
    });
  });

});
