const test = require('tape');
const sinon = require('sinon');

const ServiceLocator = require('../src/ServiceLocator');
const WhatsdoneApiClient = require('../src/WhatsdoneApiClient');

test('WhatsdoneApiClient login with user information', t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();
  apiClient.login({email: 'EMAIL@SAMPLE.COM', password: 'PASSWORD'}).then(() => {
    t.deepEqual(smartFetch.args[1], [
      'https://api_origin/signin',
      {
        method: 'POST',
        mode: 'cors',
        credentials: 'include',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
        },
        body: 'email=EMAIL%40SAMPLE.COM&password=PASSWORD'
      }
    ]);
  });

});

test('WhatsdoneApiClient fetch all done items', t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();
  apiClient.getDones().then(() => {
    t.deepEqual(smartFetch.args[1], [
      'https://api_origin/dones',
      {
        mode: 'cors',
        credentials: 'include'
      }
    ]);
  });
});

test('WhatsdoneApiClient record new done item', t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();
  apiClient.postDone({KEY: 'VALUE'}).then(() => {
    t.deepEqual(smartFetch.args[1], [
      'https://api_origin/dones',
      {
        method: 'POST',
        mode: 'cors',
        credentials: 'include',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
        },
        body: 'KEY=VALUE'
      }
    ]);
  });
});

test('WhatsdoneApiClient deletes one done item', t => {
  t.plan(1);

  const smartFetch = fakeSmartFetch();
  const apiClient = new WhatsdoneApiClient();
  apiClient.deleteDone('DONE_ID').then(() => {
    t.deepEqual(smartFetch.args[1], [
      'https://api_origin/dones/DONE_ID',
      {
        method: 'DELETE',
        mode: 'cors',
        credentials: 'include'
      }
    ]);
  });
});

function fakeSmartFetch() {
  const stub = sinon.stub();
  ServiceLocator.load({
    createSmartFetch: () => stub.returns(Promise.resolve({API_ORIGIN: 'https://api_origin'}))
  });
  return stub;
}
