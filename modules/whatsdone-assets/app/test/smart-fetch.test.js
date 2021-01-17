import 'babel-polyfill';
import test from 'tape';
import sinon from 'sinon';

import ServiceLocator from '../src/service-locator';
import smartFetch from '../src/smart-fetch';

test('smartFetch fetches json data', async t => {
  t.plan(1);

  const fakeFetch = createFakeFetch();
  ServiceLocator.load({
    createFetch: () => fakeFetch
  });

  const response = await smartFetch('URL')

  t.deepEqual(response.body, {DATA: '..'});
});

test('smartFetch treats response body as text if no content-type is specified', async t => {
  t.plan(1);

  const fakeFetch = createFakeFetch({contentType: null});
  ServiceLocator.load({
    createFetch: () => fakeFetch
  });

  const response = await smartFetch('URL')

  t.deepEqual(response.body, '{"DATA": ".."}');
});

function createFakeFetch({contentType} = {}) {
  const responseBody = '{"DATA": ".."}';
  const responseHeaders = {
    'Content-Type': typeof contentType !== 'undefined' ? contentType : 'application/json'
  };
  const response = {
    headers: {
      get: headerName => responseHeaders[headerName]
    },
    json: () => Promise.resolve(JSON.parse(responseBody)),
    text: () => Promise.resolve(responseBody)
  };
  return sinon.stub().returns(Promise.resolve(response));
}
