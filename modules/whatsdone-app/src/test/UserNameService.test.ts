import UserNameService from '../lib/UserNameService';
import ServiceLocator from '../lib/ServiceLocator';
import {expect} from 'chai';
import ServiceFactory from '../lib/ServiceFactory';
import CognitoUserFinder from '../lib/CognitoUserFinder';
import UserIdRepository from '../lib/repositories/UserId';
import * as td from 'testdouble';

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

  it('looks up a user', () => {
    return service.getUsernames(['ID']).then(result => {
      expect(result).to.eql([{
        id: 'ID',
        name: 'USER_NAME'
      }]);
    });
  });
});
