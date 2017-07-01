
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
