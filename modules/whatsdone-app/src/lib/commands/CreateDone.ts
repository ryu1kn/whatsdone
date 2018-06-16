import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';
import {Done} from '../models/Done';

export default class CreateDoneCommand {
  private _doneRepository: DoneRepository;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(data: Done, userId: string) {
    const writeParams = Object.assign({}, data, {userId});
    return this._doneRepository.write(writeParams);
  }

}
