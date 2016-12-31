
const ServiceLocator = require('../../../src/server/ServiceLocator');
const UserRepository = require('../../../src/server/repositories/User');

describe('Server UserRepository', () => {

  it('finds a user', () => {
    const userDynamoTableClient = {
      getByQuery: sinon.stub().returns(Promise.resolve('USER'))
    };
    ServiceLocator.load({
      getUserDynamoTableClient: () => userDynamoTableClient,
      getHashGenerator: () => ({generate: () => 'HASH'})
    });
    const userRepository = new UserRepository();

    const loginInfo = {
      email: 'EMAIL_ADDRESS',
      password: 'PASSWORD'
    };
    return userRepository.findUser(loginInfo).then(user => {
      expect(user).to.eql('USER');
      expect(userDynamoTableClient.getByQuery).to.have.been.calledWith({
        email: 'EMAIL_ADDRESS',
        password: 'HASH'
      });
    });
  });

});
