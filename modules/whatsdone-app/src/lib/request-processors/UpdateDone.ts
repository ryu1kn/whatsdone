import ServiceLocator from '../ServiceLocator';
import UpdateDoneCommand from '../commands/UpdateDone';
import {RequestProcessor} from '../RequestProcessor';
import {Session} from '../LambdaRequestHandler';
import {Request} from '../LambdaRequestNormaliser';

export default class UpdateDoneRequestProcessor implements RequestProcessor {
  private _updateDoneCommand: UpdateDoneCommand;

  constructor() {
    this._updateDoneCommand = ServiceLocator.updateDoneCommand;
  }

  async process(request: Request, session: Session) {
    const userId = session.userId;
    if (!userId) return {statusCode: '403'};

    const doneId = request.params.id;
    const data = request.body;
    const result = await this._updateDoneCommand.execute(data, doneId, userId);
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
