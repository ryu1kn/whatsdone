import 'babel-polyfill';
import test from 'tape';
import * as td from 'testdouble';

import ServiceLocator from '../src/service-locator';
import WhatsdoneApiClient from '../src/whatsdone-api-client';
import ServiceFactory from '../src/service-factory';

const baseOption = {
  mode: 'cors',
  credentials: 'include',
  headers: {Authorization: 'JWT_ID_TOKEN'}
};

const smartFetch = td.function();
td.when(smartFetch('/appConfig.json')).thenResolve({body: {API_ORIGIN: 'https://api_origin'}});
td.when(smartFetch('https://api_origin/dones', baseOption)).thenResolve('RESPONSE_1');
td.when(smartFetch('https://api_origin/dones?nextKey=NEXT%20KEY', baseOption)).thenResolve('RESPONSE_2');
td.when(smartFetch('https://api_origin/dones', Object.assign({}, baseOption, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8',
    Authorization: 'JWT_ID_TOKEN'
  },
  body: 'KEY=VALUE'
}))).thenResolve('RESPONSE_3');
td.when(smartFetch('https://api_origin/dones/DONE_ID', Object.assign({}, baseOption, {method: 'DELETE'})))
  .thenResolve(('RESPONSE_4'));

const createWhatsdoneApiClient = () => {
  ServiceLocator.load(new ServiceFactory(), {
    createSmartFetch: () => smartFetch,
    createCognitoUserInitialiser: () => ({}),
    createCookieStorage: () => ({
      getItem: () => 'JWT_ID_TOKEN'
    })
  });
  return new WhatsdoneApiClient();
};

test('WhatsdoneApiClient fetch all done items', async t => {
  t.plan(1);

  const apiClient = createWhatsdoneApiClient();

  const response = await apiClient.getDones()

  t.deepEqual(response, 'RESPONSE_1');
});

test('WhatsdoneApiClient fetch done items by sending a key', async t => {
  t.plan(1);

  const apiClient = createWhatsdoneApiClient();

  const response = await apiClient.getDones('NEXT KEY')

  t.deepEqual(response, 'RESPONSE_2');
});

test('WhatsdoneApiClient record new done item', async t => {
  t.plan(1);

  const apiClient = createWhatsdoneApiClient();

  const response = await apiClient.postDone({KEY: 'VALUE'})

  t.deepEqual(response, 'RESPONSE_3');
});

test('WhatsdoneApiClient deletes one done item', async t => {
  t.plan(1);

  const apiClient = createWhatsdoneApiClient();

  const response = await apiClient.deleteDone('DONE_ID')

  t.deepEqual(response, 'RESPONSE_4');
});
