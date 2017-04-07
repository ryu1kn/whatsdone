
'use strict';

const ServiceLocator = require('../ServiceLocator');

class UpdateDoneRequestProcessor {

  constructor() {
    this._updateDoneCommand = ServiceLocator.updateDoneCommand;
  }

  process(request, session) {
    const params = {
      doneId: request.params.id,
      userId: session.userId,
      data: request.body
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
