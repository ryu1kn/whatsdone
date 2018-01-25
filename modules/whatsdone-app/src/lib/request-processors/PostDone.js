
const ServiceLocator = require('../ServiceLocator');

class PostDonesRequestProcessor {

  constructor() {
    this._createDoneCommand = ServiceLocator.createDoneCommand;
  }

  process(request, session) {
    const params = {
      data: request.body,
      userId: session.userId,
      username: session.username
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
