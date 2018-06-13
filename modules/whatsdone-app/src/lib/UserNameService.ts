import ServiceLocator from './ServiceLocator';
import UserIdRepository from './repositories/UserId';

export default class UserNameService {
  private _userIdRepository: UserIdRepository;
  private _cognitoUserFinder: any;

  constructor() {
    this._userIdRepository = ServiceLocator.userIdRepository;
    this._cognitoUserFinder = ServiceLocator.cognitoUserFinder;
  }

  async getUsernames(ids) {
    const cognitoUserIds = await Promise.all(ids.map(id => this._userIdRepository.getCognitoUserId(id)));
    const finalCognitoUserIds = cognitoUserIds.map((cognitoUserId, index) => cognitoUserId || ids[index]);
    const usernames = await Promise.all(finalCognitoUserIds.map(id => this._resolveUserName(id)));
    return usernames.map((name, index) => ({
      id: ids[index],
      name
    }));
  }

  async _resolveUserName(cognitoUserId) {
    const user = await this._cognitoUserFinder.find(cognitoUserId);
    return user && user.Username;
  }

}
