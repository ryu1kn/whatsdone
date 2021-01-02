import GetDonesCommand from '../../lib/commands/GetDones';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import * as td from 'testdouble';
import UserNameService from '../../lib/UserNameService';
import DoneRepository from '../../lib/repositories/Done';
import {deepStrictEqual} from 'assert';

describe('Server GetDonesCommand', () => {
  const doneItem = {
    id: 'ID',
    date: 'DATE',
    month: 'MONTH',
    userId: 'USER_ID',
    doneThing: '..'
  };

  const userNameService = td.object('getUsernames') as UserNameService;
  td.when(userNameService.getUsernames(['USER_ID'])).thenResolve([{id: 'USER_ID', name: 'USER'}]);

  const doneRepository = td.object('read') as DoneRepository;
  td.when(doneRepository.read(undefined)).thenResolve({items: [doneItem]});

  ServiceLocator.load({
    createUserNameService: () => userNameService,
    createDoneRepository: () => doneRepository
  } as ServiceFactory);
  const command = new GetDonesCommand();

  it('returns list of dones with the names of their owners', async () => {
    const result = await command.execute();
    deepStrictEqual(result.items, [{...doneItem, username: 'USER'}]);
  });

});
