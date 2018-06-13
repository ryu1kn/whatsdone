
import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';

class DeleteDoneRequestHandler {
  private _doneRepository: DoneRepository;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async process(request, session) {
    await this._doneRepository.remove(request.params.id, session.userId);
    return {statusCode: '200'};
  }

}

export = DeleteDoneRequestHandler;
