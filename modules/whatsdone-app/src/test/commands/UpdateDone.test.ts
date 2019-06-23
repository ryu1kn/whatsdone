import UpdateDoneCommand from '../../lib/commands/UpdateDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from 'chai';
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

  it('returns updated done item with all internal data stripped', async () => {
    ServiceLocator.load({
      createDoneRepository: () => ({
        update: () => Promise.resolve(doneUpdated)
      })
    } as ServiceFactory);
    const command = new UpdateDoneCommand();

    const result = await command.execute(doneDiff, 'DONE_ID', 'USER_ID');
    expect(result).to.eql({
      doneThing: 'DONE_THING',
      date: 'DATE',
      id: 'DONE_ID',
      userId: 'USER_ID',
    });
  });
});

