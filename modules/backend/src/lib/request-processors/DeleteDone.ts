import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';
import {RequestProcessor} from '../RequestProcessor';
import {Session} from '../LambdaRequestHandler';
import {Request} from '../LambdaRequestNormaliser';

export default class DeleteDoneRequestHandler implements RequestProcessor {
  private _doneRepository: DoneRepository;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async process(request: Request, session: Session) {
    if (!session.userId) return {statusCode: '403'};
    await this._doneRepository.remove(request.params.id, session.userId);
    return {statusCode: '200'};
  }

}
