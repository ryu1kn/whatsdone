
const ServiceLocator = require('./ServiceLocator');
const url = require('url');

class LambdaResponseFormatter {

  constructor() {
    this._config = ServiceLocator.config;
  }

  format(response) {
    const headers = Object.assign({}, response.headers, {
      'Access-Control-Allow-Headers': 'Content-Type,X-Amz-Date,Authorization,X-Api-Key',
      'Access-Control-Allow-Methods': '*',
      'Access-Control-Allow-Credentials': 'true',
      'Access-Control-Allow-Origin': url.format({hostname: this._config.webappDomain, protocol: 'https'})
    });
    return Object.assign({}, response, {headers});
  }

}

module.exports = LambdaResponseFormatter;
