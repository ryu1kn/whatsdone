
'use strict';

const ServiceLocator = require('../ServiceLocator');

class PostDonesRequestProcessor {

  constructor() {
    this._createDoneCommand = ServiceLocator.createDoneCommand;
  }

  process(request) {
    const params = {
      data: request.body,
      userId: request.session.userId
    };
    return this._createDoneCommand.execute(params).then(result => {
      return {
        statusCode: '200',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify(result)
      };
    });
  }

}

module.exports = PostDonesRequestProcessor;
