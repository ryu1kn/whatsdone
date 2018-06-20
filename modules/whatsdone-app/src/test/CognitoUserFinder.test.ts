import ServiceLocator from '../lib/ServiceLocator';
import CognitoUserFinder from '../lib/CognitoUserFinder';
import {expect} from './helper/TestUtils';
import * as td from 'testdouble';
import ServiceFactory from '../lib/ServiceFactory';
import {CognitoIdentityServiceProvider} from 'aws-sdk';

describe('Server CognitoUserFinder', () => {

  it('uses CognitoIdentityServiceProvider to get the first matching user\'s info', () => {
    const cognitoIdentityServiceProvider = createCognitoIdentityServiceProvider();
    const userFinder = createCognitoUserFinder(cognitoIdentityServiceProvider);
    return userFinder.find('ID').then(user => {
      expect(user).to.eql('USER_1');
    });
  });

  function createCognitoIdentityServiceProvider() {
    const cognito = td.object('listUsers') as CognitoIdentityServiceProvider;
    td.when(cognito.listUsers({
      UserPoolId: 'USER_POOL_ID',
      AttributesToGet: [],
      Filter: 'sub = "ID"'
    })).thenReturn({
      promise: () => Promise.resolve({Users: ['USER_1']})
    });
    return cognito;
  }

  function createCognitoUserFinder(cognitoIdentityServiceProvider: CognitoIdentityServiceProvider) {
    ServiceLocator.load({
      createCognitoIdentityServiceProvider: () => cognitoIdentityServiceProvider,
      createConfig: () => ({userPoolId: 'USER_POOL_ID'})
    } as ServiceFactory);
    return new CognitoUserFinder();
  }

});
