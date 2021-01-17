import 'babel-polyfill';
import test from 'tape';
import sinon from 'sinon';

import ServiceLocator from '../src/service-locator';
import WhatsdoneApiClient from '../src/whatsdone-api-client';
import ServiceFactory from '../src/service-factory';

test('WhatsdoneApiClient fetch all done items', async t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();

  await apiClient.getDones()

  t.deepEqual(smartFetch.args[2], [
    'https://api_origin/dones',
    {
      mode: 'cors',
      credentials: 'include',
      headers: {
        Authorization: 'JWT_ID_TOKEN'
      }
    }
  ]);
});

test('WhatsdoneApiClient fetch done items by sending a key', async t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();

  await apiClient.getDones('NEXT KEY')

  t.deepEqual(smartFetch.args[2][0], 'https://api_origin/dones?nextKey=NEXT%20KEY');
});

test('WhatsdoneApiClient record new done item', async t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();

  await apiClient.postDone({KEY: 'VALUE'})

  t.deepEqual(smartFetch.args[2], [
    'https://api_origin/dones',
    {
      method: 'POST',
      mode: 'cors',
      credentials: 'include',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8',
        Authorization: 'JWT_ID_TOKEN'
      },
      body: 'KEY=VALUE'
    }
  ]);
});

test('WhatsdoneApiClient deletes one done item', async t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();

  await apiClient.deleteDone('DONE_ID')

  t.deepEqual(smartFetch.args[2], [
    'https://api_origin/dones/DONE_ID',
    {
      method: 'DELETE',
      mode: 'cors',
      credentials: 'include',
      headers: {
        Authorization: 'JWT_ID_TOKEN'
      },
    }
  ]);
});

function fakeSmartFetch() {
  const stub = sinon.stub();
  const response = {
    status: 200,
    body: {API_ORIGIN: 'https://api_origin'}
  };
  ServiceLocator.load(new ServiceFactory(), {
    createSmartFetch: () => stub.returns(Promise.resolve(response)),
    createCognitoUserInitialiser: () => ({}),
    createCookieStorage: () => ({
      getItem: () => 'JWT_ID_TOKEN'
    })
  });
  return stub;
}
