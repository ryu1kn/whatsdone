
import ServiceLocator from '../ServiceLocator';

class DeleteDoneRequestHandler {
  private _doneRepository: any;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async process(request, session) {
    await this._doneRepository.remove(request.params.id, session.userId);
    return {statusCode: '200'};
  }

}

export = DeleteDoneRequestHandler;
