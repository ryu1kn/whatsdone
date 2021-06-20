import 'babel-polyfill';
import * as td from 'testdouble';

import ServiceLocator from '../src/service-locator';
import smartFetch from '../src/smart-fetch';

const responseBody = '{"DATA": ".."}';
const defaultReqOptions = {headers: {'Accept-Encoding': 'gzip, deflate'}};

const fetch = td.function();

td.when(fetch('URL_FOR_JSON_RESPONSE', defaultReqOptions)).thenResolve({
  headers: {get: headerName => headerName === 'Content-Type' && 'application/json'},
  json: () => Promise.resolve(JSON.parse(responseBody)),
});
td.when(fetch('URL_FOR_TEXT_RESPONSE', defaultReqOptions)).thenResolve({
  headers: {get: () => {}},
  text: () => Promise.resolve(responseBody)
});

ServiceLocator.load({createFetch: () => fetch});

test('smartFetch fetches json data', async () => {
  const response = await smartFetch('URL_FOR_JSON_RESPONSE')

  expect(response.body).toEqual({DATA: '..'})
});

test('smartFetch treats response body as text if no content-type is specified', async () => {
  const response = await smartFetch('URL_FOR_TEXT_RESPONSE')

  expect(response.body).toEqual('{"DATA": ".."}')
});