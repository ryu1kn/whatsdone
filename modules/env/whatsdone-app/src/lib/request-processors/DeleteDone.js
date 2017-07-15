
'use strict';

const ServiceLocator = require('../ServiceLocator');

class DeleteDoneRequestHandler {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  process(request, session) {
    return this._doneRepository.remove(request.params.id, session.userId)
      .then(() => ({statusCode: '200'}));
  }

}

module.exports = DeleteDoneRequestHandler;
