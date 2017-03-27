
'use strict';

const ServiceLocator = require('../ServiceLocator');

class SignoutRequestProcessor {

  constructor() {
    this._sessionRepository = ServiceLocator.sessionRepository;
  }

  process(request, session) {
    return this._sessionRepository.remove(session.id)
      .then(() => ({
        statusCode: '303',
        headers: {
          Location: '/signin'
        }
      }));
  }

}

module.exports = SignoutRequestProcessor;
