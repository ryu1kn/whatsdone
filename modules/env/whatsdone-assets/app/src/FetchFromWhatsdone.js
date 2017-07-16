
const url = require('url');

const DEFAULT_OPTIONS = {
  mode: 'cors',
  credentials: 'include'
};
const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};

// HACK: Temporalily placing the retrieval of application config here
const promiseOfConfig = fetch('/appConfig.json')
  .then(response => response.json());

module.exports = (path, options = {}) => {
  const headers = Object.assign({}, DEFAULT_HEADERS, options.headers);
  const finalOptions = Object.assign({}, DEFAULT_OPTIONS, options, {headers});
  return promiseOfConfig.then(appConfig => {
    const uri = url.resolve(appConfig.API_ORIGIN, path);
    return fetch(uri, finalOptions)
      .then(response => parse(response));
  });
};

function parse(response) {
  const contentType = response.headers.get('Content-Type').toLowerCase();
  return contentType.startsWith('application/json') ? response.json() : response.text();
}
