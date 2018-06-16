import CreateDoneCommand from '../../lib/commands/CreateDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect, stubWithArgs} from '../helper/TestUtils';
import ServiceFactory from '../../lib/ServiceFactory';

describe('Server CreateDoneCommand', () => {

  it('returns list of dones with the names of their owners', () => {
    const done = {doneThing: 'SOMETHING', date: 'DATE'};
    const doneWithUserId = Object.assign({}, done, {userId: 'USER_ID'});
    const doneRepository = {
      write: stubWithArgs(
        [doneWithUserId],
        Promise.resolve(doneWithUserId)
      )
    };
    ServiceLocator.load({
      createDoneRepository: () => doneRepository
    } as ServiceFactory);
    const command = new CreateDoneCommand();

    return command.execute(done, 'USER_ID').then(result => {
      expect(result).to.eql(doneWithUserId);
    });
  });

});

