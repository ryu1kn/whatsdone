import CreateDoneCommand from '../../lib/commands/CreateDone';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import * as td from 'testdouble';
import DoneRepository from '../../lib/repositories/Done';
import {deepStrictEqual} from 'assert';

describe('Server CreateDoneCommand', () => {

  it('returns list of dones with the names of their owners', async () => {
    const done = {doneThing: 'SOMETHING', date: 'DATE'};
    const doneWithUserId = Object.assign({}, done, {userId: 'USER_ID'});
    const doneRepository = td.object('write') as DoneRepository;
    td.when(doneRepository.write(doneWithUserId)).thenResolve(doneWithUserId);

    ServiceLocator.load({
      createDoneRepository: () => doneRepository
    } as ServiceFactory);
    const command = new CreateDoneCommand();

    const result = await command.execute(done, 'USER_ID');
    deepStrictEqual(result, doneWithUserId);
  });

});

