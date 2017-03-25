

const ServiceLocator = require('../../../src/server/ServiceLocator');
const SessionRepository = require('../../../src/server/repositories/Session');

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

});
