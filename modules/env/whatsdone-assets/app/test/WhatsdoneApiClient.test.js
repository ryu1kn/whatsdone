const test = require('tape');
const sinon = require('sinon');

const ServiceLocator = require('../src/ServiceLocator');
const WhatsdoneApiClient = require('../src/WhatsdoneApiClient');

test('WhatsdoneApiClient login with user information', t => {
  t.plan(1);

  const fetchFromWhatsdone = fakeFetchFromWhatsdone();
  const apiClient = new WhatsdoneApiClient();
  apiClient.login({email: 'EMAIL@SAMPLE.COM', password: 'PASSWORD'});

  t.deepEqual(fetchFromWhatsdone.args[0], [
    '/signin',
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
      },
      body: 'email=EMAIL%40SAMPLE.COM&password=PASSWORD'
    }
  ]);
});

test('WhatsdoneApiClient fetch all done items', t => {
  t.plan(1);

  const fetchFromWhatsdone = fakeFetchFromWhatsdone();
  const apiClient = new WhatsdoneApiClient();
  apiClient.getDones();

  t.deepEqual(fetchFromWhatsdone.args[0], ['/dones']);
});

test('WhatsdoneApiClient record new done item', t => {
  t.plan(1);

  const fetchFromWhatsdone = fakeFetchFromWhatsdone();
  const apiClient = new WhatsdoneApiClient();
  apiClient.postDone({KEY: 'VALUE'});

  t.deepEqual(fetchFromWhatsdone.args[0], [
    '/dones',
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
      },
      body: 'KEY=VALUE'
    }
  ]);
});

test('WhatsdoneApiClient deletes one done item', t => {
  t.plan(1);

  const fetchFromWhatsdone = fakeFetchFromWhatsdone();
  const apiClient = new WhatsdoneApiClient();
  apiClient.deleteDone('DONE_ID');

  t.deepEqual(fetchFromWhatsdone.args[0], [
    '/dones/DONE_ID',
    {method: 'DELETE'}
  ]);
});

function fakeFetchFromWhatsdone() {
  const spy = sinon.spy();
  ServiceLocator.load({
    createFetchFromWhatsdone: () => spy
  });
  return spy;
}
