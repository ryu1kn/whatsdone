import ServiceLocator from './service-locator';

const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};

export type SmartFetchResponse = { status: number, body: any }

export type SmartFetch = (uri: string, options?: RequestInit) => Promise<SmartFetchResponse>;

export default (uri: string, options?: RequestInit) => {
  const headers = Object.assign({}, DEFAULT_HEADERS, options?.headers);
  const finalOptions = Object.assign({}, options, {headers});
  return ServiceLocator.fetch(uri, finalOptions).then(parse);
};

function parse(response: Response): Promise<SmartFetchResponse> {
  const promiseOfBody = shouldBeJson(response) ? response.json() : response.text();
  return promiseOfBody.then(body => ({
    status: response.status,
    body
  }));
}

function shouldBeJson(response: Response) {
  const contentType = response.headers.get('Content-Type');
  return contentType ? contentType.toLowerCase().startsWith('application/json') : false;
}
