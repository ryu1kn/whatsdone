
import ServiceLocator = require('../ServiceLocator');

class UpdateDoneRequestProcessor {
  private _updateDoneCommand: any;

  constructor() {
    this._updateDoneCommand = ServiceLocator.updateDoneCommand;
  }

  async process(request, session) {
    const params = {
      doneId: request.params.id,
      userId: session.userId,
      data: request.body
    };
    const result = await this._updateDoneCommand.execute(params);
    return {
      statusCode: '200',
      headers: {
        'Content-Type': 'application/json',
        'Cache-Control': 'no-cache'
      },
      body: JSON.stringify(result)
    };
  }

}

export = UpdateDoneRequestProcessor;
