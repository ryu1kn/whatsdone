
'use strict';

const ServiceLocator = require('../ServiceLocator');

class GetDonesRequestProcessor {

  constructor() {
    this._getDonesCommand = ServiceLocator.getDonesCommand;
  }

  process(_request) {
    return this._getDonesCommand.execute().then(dones => ({
      statusCode: '200',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify(dones)
    }));
  }

}

module.exports = GetDonesRequestProcessor;
