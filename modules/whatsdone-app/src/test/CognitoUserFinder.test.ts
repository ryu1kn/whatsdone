import ServiceLocator from '../lib/ServiceLocator';
import CognitoUserFinder from '../lib/CognitoUserFinder';
import {expect} from './TestUtils';
import sinon = require('sinon');
import ServiceFactory from '../lib/ServiceFactory';

describe('Server CognitoUserFinder', () => {

  it('uses CognitoIdentityServiceProvider to get user info', () => {
    const cognitoIdentityServiceProvider = createCognitoIdentityServiceProvider();
    const userFinder = createCognitoUserFinder(cognitoIdentityServiceProvider);

    return userFinder.find(['ID']).then(() => {
      expect(cognitoIdentityServiceProvider.listUsers).to.have.been.calledWith({
        UserPoolId: 'USER_POOL_ID',
        AttributesToGet: [],
        Filter: 'sub = "ID"'
      });
    });
  });

  it('returns first matching user', () => {
    const cognitoIdentityServiceProvider = createCognitoIdentityServiceProvider();
    const userFinder = createCognitoUserFinder(cognitoIdentityServiceProvider);

    return userFinder.find(['ID']).then(user => {
      expect(user).to.eql('USER_1');
    });
  });

  function createCognitoIdentityServiceProvider() {
    return {
      listUsers: sinon.stub().returns({
        promise: () => Promise.resolve({Users: ['USER_1']})
      })
    };
  }

  function createCognitoUserFinder(cognitoIdentityServiceProvider) {
    ServiceLocator.load({
      createCognitoIdentityServiceProvider: () => cognitoIdentityServiceProvider,
      createConfig: () => ({userPoolId: 'USER_POOL_ID'})
    } as ServiceFactory);
    return new CognitoUserFinder();
  }

});
