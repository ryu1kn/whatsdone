
const ServiceLocator = require('../../lib/ServiceLocator');
const UserRepository = require('../../lib/repositories/User');

describe('Server UserRepository', () => {

  it('finds a user by a user id', () => {
    const userDynamoTableClient = {
      getById: sinon.stub().returns(Promise.resolve('USER'))
    };
    ServiceLocator.load({
      createUserDynamoTableClient: () => userDynamoTableClient
    });
    const userRepository = new UserRepository();

    return userRepository.getById('USER_ID').then(user => {
      expect(user).to.eql('USER');
      expect(userDynamoTableClient.getById).to.have.been.calledWith('USER_ID');
    });
  });

  it('finds users by user IDs', () => {
    const userDynamoTableClient = {
      getByIds: sinon.stub().returns(Promise.resolve(['USER_1', 'USER_2']))
    };
    ServiceLocator.load({
      createUserDynamoTableClient: () => userDynamoTableClient
    });
    const userRepository = new UserRepository();

    return userRepository.getByIds(['USER_ID_1', 'USER_ID_2']).then(users => {
      expect(users).to.eql(['USER_1', 'USER_2']);
      expect(userDynamoTableClient.getByIds).to.have.been.calledWith(
        ['USER_ID_1', 'USER_ID_2']
      );
    });
  });

});
