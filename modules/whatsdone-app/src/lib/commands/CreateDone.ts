import ServiceLocator from '../ServiceLocator';
import DoneRepository from '../repositories/Done';
import {Done, DoneInDb} from '../models/Done';

export default class CreateDoneCommand {
  private _doneRepository: DoneRepository;

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(data: Done, userId: string) {
    const newDone = await this._doneRepository.write({...data, userId});
    return {...newDone, topics: ["foo"]};
  }
}
