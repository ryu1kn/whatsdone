import GetDonesCommand from '../../lib/commands/GetDones';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import * as td from 'testdouble';
import UserNameService from '../../lib/UserNameService';
import DoneRepository from '../../lib/repositories/Done';
import {deepStrictEqual} from 'assert';

describe('Server GetDonesCommand', () => {
  const doneWithTopic = {
    id: 'ID',
    date: 'DATE',
    month: 'MONTH',
    userId: 'USER_ID',
    doneThing: 'SOMETHING',
    topics: ['foo'],
  };
  const doneWithoutTopic = {
    id: 'ID',
    date: 'DATE',
    month: 'MONTH',
    userId: 'USER_ID',
    doneThing: 'SOMETHING_ELSE',
  };

  const userNameService = td.object('getUsernames') as UserNameService;
  td.when(userNameService.getUsernames(['USER_ID', 'USER_ID'])).thenResolve([{id: 'USER_ID', name: 'USER'}]);

  const doneRepository = td.object('read') as DoneRepository;
  td.when(doneRepository.read(undefined)).thenResolve({items: [doneWithTopic, doneWithoutTopic]});

  ServiceLocator.load({
    createUserNameService: () => userNameService,
    createDoneRepository: () => doneRepository,
  } as ServiceFactory);
  const command = new GetDonesCommand();

  it('returns list of dones with the names of their owners', async () => {
    const result = await command.execute();
    deepStrictEqual(result.items, [
      {...doneWithTopic, username: 'USER'},
      {...doneWithoutTopic, username: 'USER', topics: []},
    ]);
  });

});
