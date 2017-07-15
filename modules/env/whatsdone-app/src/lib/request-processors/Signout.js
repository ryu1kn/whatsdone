
const ServiceLocator = require('../ServiceLocator');

class SignoutRequestProcessor {

  constructor() {
    this._sessionRepository = ServiceLocator.sessionRepository;
  }

  process(_request, session) {
    return this._sessionRepository.remove(session.id)
      .then(() => ({
        statusCode: '200'
      }));
  }

}

module.exports = SignoutRequestProcessor;
