import ServiceLocator from '../lib/ServiceLocator';
import CognitoUserFinder from '../lib/CognitoUserFinder';
import * as td from 'testdouble';
import ServiceFactory from '../lib/ServiceFactory';
import {AWSError, CognitoIdentityServiceProvider} from 'aws-sdk';
import {PromiseResult} from 'aws-sdk/lib/request';
import {ListUsersResponse} from 'aws-sdk/clients/cognitoidentityserviceprovider';
import {deepStrictEqual} from 'assert';

describe('Server CognitoUserFinder', () => {

  it('uses CognitoIdentityServiceProvider to get the first matching user\'s info', async () => {
    const cognitoIdentityServiceProvider = createCognitoIdentityServiceProvider();
    const userFinder = createCognitoUserFinder(cognitoIdentityServiceProvider);
    const user = await userFinder.find('ID');
    deepStrictEqual(user, 'USER_1');
  });

  function createCognitoIdentityServiceProvider() {
    const cognito = td.object('listUsers') as CognitoIdentityServiceProvider;
    td.when(cognito.listUsers({
      UserPoolId: 'USER_POOL_ID',
      AttributesToGet: [],
      Filter: 'sub = "ID"'
    })).thenReturn({
      promise: () => Promise.resolve({Users: ['USER_1']} as unknown as PromiseResult<ListUsersResponse, AWSError>)
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
