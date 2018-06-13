
import ServiceLocator from '../ServiceLocator';

class PostDonesRequestProcessor {
  private _createDoneCommand: any;

  constructor() {
    this._createDoneCommand = ServiceLocator.createDoneCommand;
  }

  async process(request, session) {
    const params = {
      data: request.body,
      userId: session.userId,
      username: session.username
    };
    const result = await this._createDoneCommand.execute(params);
    return {
      statusCode: '200',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify(result)
    };
  }

}

export = PostDonesRequestProcessor;
