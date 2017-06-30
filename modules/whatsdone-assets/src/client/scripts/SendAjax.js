
const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};

module.exports = (url, options = {}) => {
  const headers = Object.assign({}, options.headers, DEFAULT_HEADERS);
  const finalOptions = Object.assign({}, options, {headers});
  return fetch(url, finalOptions).then(parse);
};

function parse(response) {
  const contentType = response.headers.get('Content-Type').toLowerCase();
  return contentType === 'application/json' ? response.json() : response.text();
}
