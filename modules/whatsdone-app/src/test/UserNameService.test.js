
const UserNameService = require('../lib/UserNameService');
const ServiceLocator = require('../lib/ServiceLocator');

describe('Server UserNameService', () => {

  it('finds cognito user ids from given ids', () => {
    const userIdRepository = {getCognitoUserId: sinon.stub().returns(Promise.resolve('USER_ID'))};
    const cognitoUserFinder = {find: () => Promise.resolve({})};
    const service = createUserNameService({cognitoUserFinder, userIdRepository});

    return service.getUsernames(['ID']).then(() => {
      expect(userIdRepository.getCognitoUserId).to.have.been.calledWith('ID');
    });
  });

  it('looks up a user', () => {
    const userIdRepository = {getCognitoUserId: () => Promise.resolve('USER_ID')};
    const cognitoUserFinder = {find: () => Promise.resolve({Username: 'USER_NAME'})};
    const service = createUserNameService({cognitoUserFinder, userIdRepository});

    return service.getUsernames(['ID']).then(result => {
      expect(result).to.eql([{
        id: 'ID',
        name: 'USER_NAME'
      }]);
    });
  });

  function createUserNameService({cognitoUserFinder, userIdRepository}) {
    ServiceLocator.load({
      createUserIdRepository: () => userIdRepository,
      createCognitoUserFinder: () => cognitoUserFinder
    });
    return new UserNameService();
  }

});
