import ServiceLocator from './service-locator';

interface RequestOptions {
  headers?: {[k: string]: string}
}

const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};

export default (uri, options: RequestOptions = {}) => {
  const headers = Object.assign({}, DEFAULT_HEADERS, options.headers);
  const finalOptions = Object.assign({}, options, {headers});
  return ServiceLocator.fetch(uri, finalOptions)
    .then(response => parse(response));
};

function parse(response) {
  const promiseOfBody = shouldBeJson(response) ? response.json() : response.text();
  return promiseOfBody.then(body => ({
    status: response.status,
    body
  }));
}

function shouldBeJson(response) {
  const contentType = response.headers.get('Content-Type');
  return contentType ? contentType.toLowerCase().startsWith('application/json') : false;
}
