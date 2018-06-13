
import _ = require('lodash');
import ServiceLocator from '../ServiceLocator';

class GetDonesCommand {
  private _userNameService: any;
  private _doneRepository: any;

  constructor() {
    this._userNameService = ServiceLocator.userNameService;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  async execute(nextKey?) {
    const result = await this._doneRepository.read(nextKey);
    const items = await this._setUserNames(result.items);
    return {
      items,
      nextKey: result.nextKey
    };
  }

  async _setUserNames(dones) {
    const users = await this._userNameService.getUsernames(_.map(dones, 'userId'));
    const nameMap = _.keyBy(users, 'id');
    return dones.map(done =>
      done.userId ?
        Object.assign({}, done, {username: _.get(nameMap, `${done.userId}.name`)}) :
        done
    );
  }

}

export = GetDonesCommand;
