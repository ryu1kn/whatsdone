
import test from 'tape';
import sinon from 'sinon';

import ServiceLocator from '../src/service-locator';
import smartFetch from '../src/smart-fetch';

test('smartFetch fetches json data', t => {
  t.plan(1);

  const fakeFetch = createFakeFetch();
  ServiceLocator.load({
    createFetch: () => fakeFetch
  });

  smartFetch('URL').then(_response => {
    t.deepEqual(fakeFetch.args[0][0], 'URL');
  });
});

function createFakeFetch() {
  const response = {
    headers: {
      get: function (headerName) {
        return this._headers[headerName];
      },
      _headers: {'Content-Type': 'application/json'}
    },
    json: () => Promise.resolve('{"DATA": ".."}')
  };
  return sinon.stub().returns(Promise.resolve(response));
}
