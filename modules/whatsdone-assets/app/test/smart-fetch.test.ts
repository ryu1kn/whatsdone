import 'babel-polyfill';
import * as td from 'testdouble';

import ServiceLocator from '../src/service-locator';
import smartFetch from '../src/smart-fetch';
import ServiceFactory from '../src/service-factory';

const responseBody = '{"DATA": ".."}';
const defaultReqOptions = {headers: {'Accept-Encoding': 'gzip, deflate'}};

const fetch = td.function() as (input: RequestInfo, init?: RequestInit | undefined) => Promise<Response>;

td.when(fetch('URL_FOR_JSON_RESPONSE', defaultReqOptions)).thenResolve({
  // @ts-ignore: It doesn't need to be a real Headers instance
  headers: {get: (headerName: string) => headerName === 'Content-Type' && 'application/json'},
  json: () => Promise.resolve(JSON.parse(responseBody)),
});
td.when(fetch('URL_FOR_TEXT_RESPONSE', defaultReqOptions)).thenResolve({
  // @ts-ignore: It doesn't need to be a real Headers instance
  headers: {get: () => {}},
  text: () => Promise.resolve(responseBody)
});

ServiceLocator.load({createFetch: () => fetch} as ServiceFactory);

test('smartFetch fetches json data', async () => {
  const response = await smartFetch('URL_FOR_JSON_RESPONSE')

  expect(response.body).toEqual({DATA: '..'})
});

test('smartFetch treats response body as text if no content-type is specified', async () => {
  const response = await smartFetch('URL_FOR_TEXT_RESPONSE')

  expect(response.body).toEqual('{"DATA": ".."}')
});
