import UpdateDoneCommand from '../../lib/commands/UpdateDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from '../helper/TestUtils';
import ServiceFactory from '../../lib/ServiceFactory';

describe('Server UpdateDoneCommand', () => {

  const doneDiff = {
    doneThing: 'DONE_THING',
    date: 'DATE'
  };
  const doneUpdated = {
    doneThing: 'DONE_THING',
    date: 'DATE',
    id: 'DONE_ID',
    userId: 'USER_ID',
    INTERNAL_DATA: '..'
  };

  it('returns updated done item with all internal data stripped', () => {
    ServiceLocator.load({
      createDoneRepository: () => ({
        update: () => Promise.resolve(doneUpdated)
      })
    } as ServiceFactory);
    const command = new UpdateDoneCommand();

    return command.execute(doneDiff, 'DONE_ID', 'USER_ID').then(result => {
      expect(result).to.eql({
        doneThing: 'DONE_THING',
        date: 'DATE',
        id: 'DONE_ID',
        userId: 'USER_ID',
      });
    });
  });
});

