import UserNameService from '../../lib/services/UserNameService';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import CognitoUserFinder from '../../lib/CognitoUserFinder';
import UserIdRepository from '../../lib/repositories/UserId';
import * as td from 'testdouble';
import {deepStrictEqual} from 'assert';

describe('Server UserNameService', () => {
  const userIdRepository = td.object('getCognitoUserId') as UserIdRepository;
  td.when(userIdRepository.getCognitoUserId('ID')).thenResolve('USER_ID');

  const cognitoUserFinder = td.object('find') as CognitoUserFinder;
  td.when(cognitoUserFinder.find('USER_ID')).thenResolve({Username: 'USER_NAME'});

  ServiceLocator.load({
    createUserIdRepository: () => userIdRepository,
    createCognitoUserFinder: () => cognitoUserFinder
  } as ServiceFactory);
  const service = new UserNameService();

  it('looks up a user', async () => {
    const result = await service.getUsernames(['ID']);
    deepStrictEqual(result, [{
      id: 'ID',
      name: 'USER_NAME'
    }]);
  });
});
