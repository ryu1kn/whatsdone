
const ServiceLocator = require('../ServiceLocator');

class DeleteDoneRequestHandler {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async process(request, session) {
    await this._doneRepository.remove(request.params.id, session.userId);
    return {statusCode: '200'};
  }

}

module.exports = DeleteDoneRequestHandler;
