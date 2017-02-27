
'use strict';

const ServiceLocator = require('../ServiceLocator');

class UpdateDoneRequestProcessor {

  constructor() {
    this._updateDoneCommand = ServiceLocator.updateDoneCommand;
  }

  process(req) {
    const params = {
      doneId: req.params.id,
      userId: req.session.userId,
      data: req.body
    };
    return this._updateDoneCommand.execute(params).then(result => ({
      statusCode: '200',
      headers: {
        'Content-Type': 'application/json',
        'Cache-Control': 'no-cache'
      },
      body: JSON.stringify(result)
    }));
  }

}

module.exports = UpdateDoneRequestProcessor;
