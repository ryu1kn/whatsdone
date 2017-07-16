
const DEFAULT_OPTIONS = {
  mode: 'cors',
  credentials: 'include'
};
const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};

module.exports = (url, options = {}) => {
  const headers = Object.assign({}, DEFAULT_HEADERS, options.headers);
  const finalOptions = Object.assign({}, DEFAULT_OPTIONS, options, {headers});
  return fetch(url, finalOptions)
    .then(response => parse(response));
};

function parse(response) {
  const contentType = response.headers.get('Content-Type').toLowerCase();
  return contentType.startsWith('application/json') ? response.json() : response.text();
}
