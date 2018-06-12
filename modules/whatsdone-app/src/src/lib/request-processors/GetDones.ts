
import ServiceLocator = require('../ServiceLocator');

class GetDonesRequestProcessor {
  private _getDonesCommand: any;

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

export = GetDonesRequestProcessor;
