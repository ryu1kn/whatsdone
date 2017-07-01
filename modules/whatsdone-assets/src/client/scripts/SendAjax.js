
const DEFAULT_HEADERS = {
  'Accept-Encoding': 'gzip, deflate'
};

module.exports = (url, options = {}) => {
  const headers = Object.assign({}, options.headers, DEFAULT_HEADERS);
  const finalOptions = Object.assign({}, options, {headers});
  return fetch(url, finalOptions)
    .then(response => parse(response, getParseOptions(options)));
};

function parse(response, parseOptions) {
  const contentType = response.headers.get('Content-Type').toLowerCase();
  if (shouldLogin(contentType, parseOptions)) {
    window.location = '/signin.html';
  }
  return contentType === 'application/json' ? response.json() : response.text();
}

function getParseOptions(options) {
  return {
    expectedContentType: (options.expectedContentType || '').toLowerCase()
  };
}

function shouldLogin(contentType, parseOptions) {
  return parseOptions.expectedContentType &&
      parseOptions.expectedContentType !== contentType;
}
