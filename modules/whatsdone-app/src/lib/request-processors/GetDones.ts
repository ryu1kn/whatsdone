import ServiceLocator from '../ServiceLocator';
import GetDonesCommand from '../commands/GetDones';

export default class GetDonesRequestProcessor {
  private _getDonesCommand: GetDonesCommand;

  constructor() {
    this._getDonesCommand = ServiceLocator.getDonesCommand;
  }

  async process(request, _session?) {
    const nextKey = request.query.nextKey;
    const dones = await this._getDonesCommand.execute(nextKey);
    return {
      statusCode: '200',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(dones)
    };
  }

}
