import ServiceLocator from '../ServiceLocator';
import UpdateDoneCommand from '../commands/UpdateDone';
import {RequestProcessor} from '../RequestProcessor';

export default class UpdateDoneRequestProcessor implements RequestProcessor {
  private _updateDoneCommand: UpdateDoneCommand;

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
