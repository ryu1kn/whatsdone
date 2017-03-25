
const LoginCommand = require('../../../src/server/commands/Login');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server LoginCommand', () => {

  it('creates a session if the user exists', () => {
    const userRepository = {
      findUser: sinon.stub().returns(Promise.resolve({id: 'USER_ID'}))
    };
    const sessionRepository = {
      write: sinon.stub().returns(Promise.resolve('SESSION_ID'))
    };
    ServiceLocator.load({
      createUserRepository: () => userRepository,
      createSessionRepository: () => sessionRepository
    });
    const command = new LoginCommand();

    const params = {
      email: 'EMAIL',
      password: 'PASSWORD'
    };
    return command.execute(params).then(sessionId => {
      expect(sessionId).to.eql('SESSION_ID');
      expect(userRepository.findUser).to.have.been.calledWith({
        email: 'EMAIL',
        password: 'PASSWORD'
      });
      expect(sessionRepository.write).to.have.been.calledWith({
        isAuthorized: true,
        userId: 'USER_ID'
      });
    });
  });

  it('does not create a session if no matching login information found', () => {
    ServiceLocator.load({
      createUserRepository: () => ({
        findUser: () => Promise.resolve(null)
      }),
      createSessionRepository: () => {}
    });
    const command = new LoginCommand();

    const params = {
      email: 'EMAIL',
      password: 'INVALID_PASSWORD'
    };
    return command.execute(params).then(session => {
      expect(session).to.eql(null);
    });
  });

});
