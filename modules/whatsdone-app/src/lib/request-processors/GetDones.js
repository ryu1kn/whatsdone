
const ServiceLocator = require('../ServiceLocator');
const url = require('url');

class GetDonesRequestProcessor {

  constructor() {
    this._config = ServiceLocator.config;
    this._getDonesCommand = ServiceLocator.getDonesCommand;
  }

  process(_request, _session) {
    return this._getDonesCommand.execute().then(dones => ({
      statusCode: '200',
      headers: {
        'Content-Type': 'application/json',
        'Access-Control-Allow-Headers': 'Content-Type,X-Amz-Date,Authorization,X-Api-Key',
        'Access-Control-Allow-Methods': '*',
        'Access-Control-Allow-Origin': url.format({hostname: this._config.webappDomain, protocol: 'https'})
      },
      body: JSON.stringify(dones)
    }));
  }

}

module.exports = GetDonesRequestProcessor;
