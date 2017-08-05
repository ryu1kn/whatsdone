
const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};

module.exports = (uri, options = {}) => {
  const headers = Object.assign({}, DEFAULT_HEADERS, options.headers);
  const finalOptions = Object.assign({}, options, {headers});
  return fetch(uri, finalOptions)
    .then(response => parse(response));
};

function parse(response) {
  const contentType = response.headers.get('Content-Type').toLowerCase();
  const promiseOfBody = contentType.startsWith('application/json') ? response.json() : response.text();
  return promiseOfBody.then(body => ({
    status: response.status,
    body
  }));
}
