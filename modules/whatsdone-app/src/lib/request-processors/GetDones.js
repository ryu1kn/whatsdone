
const ServiceLocator = require('../ServiceLocator');

class GetDonesRequestProcessor {

  constructor() {
    this._getDonesCommand = ServiceLocator.getDonesCommand;
  }

  process(request, _session) {
    const nextKey = request.query.nextKey;
    return this._getDonesCommand.execute(nextKey).then(dones => ({
      statusCode: '200',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(dones)
    }));
  }

}

module.exports = GetDonesRequestProcessor;
