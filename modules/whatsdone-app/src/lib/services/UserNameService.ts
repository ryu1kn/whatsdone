import ServiceLocator from '../ServiceLocator';
import UserIdRepository from '../repositories/UserId';
import CognitoUserFinder from '../CognitoUserFinder';

export default class UserNameService {
  private _userIdRepository: UserIdRepository;
  private _cognitoUserFinder: CognitoUserFinder;

  constructor() {
    this._userIdRepository = ServiceLocator.userIdRepository;
    this._cognitoUserFinder = ServiceLocator.cognitoUserFinder;
  }

  async getUsernames(ids: string[]) {
    const uniqueIds = ids.reduce((acc, id) => acc.includes(id) ? acc : [...acc, id], [] as string[]);
    const cognitoUserIds = await Promise.all(uniqueIds.map(id => this._userIdRepository.getCognitoUserId(id)));
    const finalCognitoUserIds = cognitoUserIds.map((cognitoUserId, index) => cognitoUserId || uniqueIds[index]);
    const usernames = await Promise.all(finalCognitoUserIds.map(id => this.resolveUserName(id)));
    return usernames.map((name, index) => ({
      id: uniqueIds[index],
      name
    }));
  }

  private async resolveUserName(cognitoUserId: string) {
    const user = await this._cognitoUserFinder.find(cognitoUserId);
    return user && user.Username;
  }
}
