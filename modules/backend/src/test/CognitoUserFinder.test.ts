import ServiceLocator from '../lib/ServiceLocator';
import CognitoUserFinder from '../lib/CognitoUserFinder';
import ServiceFactory from '../lib/ServiceFactory';
import {CognitoIdentityProviderClient, ListUsersCommand} from '@aws-sdk/client-cognito-identity-provider';
import {AwsClientStub, mockClient} from 'aws-sdk-client-mock';
import {deepStrictEqual} from 'assert';

describe('Server CognitoUserFinder', () => {
  let cognitoMock: AwsClientStub<CognitoIdentityProviderClient>;

  beforeEach(() => {
    cognitoMock = mockClient(CognitoIdentityProviderClient);
  });

  afterEach(() => cognitoMock.restore());

  it('uses CognitoIdentityServiceProvider to get the first matching user\'s info', async () => {
    cognitoMock.on(ListUsersCommand, {
      UserPoolId: 'USER_POOL_ID',
      AttributesToGet: [],
      Filter: 'sub = "ID"'
    }).resolves({Users: ['USER_1' as any]});

    const userFinder = createCognitoUserFinder();
    const user = await userFinder.find('ID');
    deepStrictEqual(user, 'USER_1');
  });

  function createCognitoUserFinder() {
    ServiceLocator.load({
      createCognitoIdentityServiceProvider: () => new CognitoIdentityProviderClient({region: 'ap-southeast-2'}),
      createConfig: () => ({userPoolId: 'USER_POOL_ID'})
    } as ServiceFactory);
    return new CognitoUserFinder();
  }

});
