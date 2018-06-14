import ServiceLocator from '../ServiceLocator';
import CreateDoneCommand from '../commands/CreateDone';
import {RequestProcessor} from '../RequestProcessor';

export default class PostDonesRequestProcessor implements RequestProcessor {
  private _createDoneCommand: CreateDoneCommand;

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