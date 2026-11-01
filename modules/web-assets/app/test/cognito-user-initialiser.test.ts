/**
 * @jest-environment jsdom
 */
import {CognitoUserInitialiser} from '../src/cognito-user-initialiser';
import ServiceLocator from '../src/service-locator';
import ServiceFactory from '../src/service-factory';
import {CookieStorage} from 'amazon-cognito-identity-js';

const jwt = (claims: object) => `e30.${btoa(JSON.stringify(claims))}.signature`;

afterEach(() => { window.location.hash = ''; });

test('stores Cognito redirect tokens with the existing cookie keys', async () => {
  const values = new Map<string, string>();
  const storage = {
    setItem: (key: string, value: string) => { values.set(key, value); },
    getItem: (key: string) => values.get(key),
    removeItem: (key: string) => { values.delete(key); },
    clear: () => values.clear()
  } as unknown as CookieStorage;
  ServiceLocator.load(new ServiceFactory(), {
    createConfigProvider: () => ({getConfig: async () => ({
      USER_POOL_ID: 'ap-southeast-2_test', CLIENT_ID: 'test-client'
    })}),
    createCookieStorage: () => storage
  } as unknown as ServiceFactory);
  const idToken = jwt({'cognito:username': 'test-user', exp: 2000000000, iat: 1900000000});
  const accessToken = jwt({exp: 2000000000, iat: 1900000000});
  window.location.hash = new URLSearchParams({id_token: idToken, access_token: accessToken}).toString();

  await new CognitoUserInitialiser().initialise();

  const prefix = 'CognitoIdentityServiceProvider.test-client';
  expect(values.get(`${prefix}.LastAuthUser`)).toBe('test-user');
  expect(values.get(`${prefix}.test-user.idToken`)).toBe(idToken);
  expect(values.get(`${prefix}.test-user.accessToken`)).toBe(accessToken);
});
