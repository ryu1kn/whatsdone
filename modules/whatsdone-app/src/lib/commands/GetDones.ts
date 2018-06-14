import _ = require('lodash');
import ServiceLocator from '../ServiceLocator';
import UserNameService from '../UserNameService';
import DoneRepository from '../repositories/Done';

export default class GetDonesCommand {
  private _userNameService: UserNameService;
  private _doneRepository: DoneRepository;

  constructor() {
    this._userNameService = ServiceLocator.userNameService;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(nextKey?) {
    const result = await this._doneRepository.read(nextKey);
    const items = await this.setUserNames(result.items);
    return {
      items,
      nextKey: result.nextKey
    };
  }

  private async setUserNames(dones) {
    const users = await this._userNameService.getUsernames(_.map(dones, 'userId'));
    const nameMap = _.keyBy(users, 'id');
    return dones.map(done =>
      done.userId ?
        Object.assign({}, done, {username: _.get(nameMap, `${done.userId}.name`)}) :
        done
    );
  }

}
