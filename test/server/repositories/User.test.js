
const ServiceLocator = require('../../../src/server/ServiceLocator');
const UserRepository = require('../../../src/server/repositories/User');

describe('Server UserRepository', () => {

  it('finds a user by an email and hashed password', () => {
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

  it('finds a user by a user id', () => {
    const userDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve('USER'))
    };
    ServiceLocator.load({
      getUserDynamoTableClient: () => userDynamoTableClient,
      getHashGenerator: () => {}
    });
    const userRepository = new UserRepository();

    return userRepository.getById('USER_ID').then(user => {
      expect(user).to.eql('USER');
      expect(userDynamoTableClient.getById).to.have.been.calledWith('USER_ID');
    });
  });

  it('finds users by user IDs', () => {
    const userDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve(['USER_1', 'USER_2']))
    };
    ServiceLocator.load({
      getUserDynamoTableClient: () => userDynamoTableClient,
      getHashGenerator: () => {}
    });
    const userRepository = new UserRepository();

    return userRepository.getById(['USER_ID_1', 'USER_ID_2']).then(users => {
      expect(users).to.eql(['USER_1', 'USER_2']);
      expect(userDynamoTableClient.getById).to.have.been.calledWith(
        ['USER_ID_1', 'USER_ID_2']
      );
    });
  });

});
