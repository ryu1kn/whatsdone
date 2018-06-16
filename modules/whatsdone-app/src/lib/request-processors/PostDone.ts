import ServiceLocator from '../ServiceLocator';
import CreateDoneCommand from '../commands/CreateDone';
import {RequestProcessor} from '../RequestProcessor';
import {Request} from '../LambdaRequestNormaliser';
import {Session} from '../LambdaRequestHandler';

export default class PostDonesRequestProcessor implements RequestProcessor {
  private _createDoneCommand: CreateDoneCommand;

  constructor() {
    this._createDoneCommand = ServiceLocator.createDoneCommand;
  }

  async process(request: Request, session: Session) {
    const result = await this._createDoneCommand.execute(request.body, session.userId);
    const finalResult = Object.assign({}, result, {username: session.username});
    return {
      statusCode: '200',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify(finalResult)
    };
  }

}
