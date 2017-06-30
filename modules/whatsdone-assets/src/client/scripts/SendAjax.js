
const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};
const StatusCode = {
  SEE_OTHER: 303
};

module.exports = (url, options = {}) => {
  const headers = Object.assign({}, options.headers, DEFAULT_HEADERS);
  const finalOptions = Object.assign({}, options, {headers});
  return fetch(url, finalOptions).then(parse);
};

function parse(response) {
  if (response.status === StatusCode.SEE_OTHER) {
    window.location = '/signin';
  }
  const contentType = response.headers.get('Content-Type').toLowerCase();
  return contentType === 'application/json' ? response.json() : response.text();
}
