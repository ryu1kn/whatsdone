
const UpdateDoneCommand = require('../../lib/commands/UpdateDone');
const ServiceLocator = require('../../lib/ServiceLocator');

describe('Server UpdateDoneCommand', () => {

  it('updates a done item in the Done repository', () => {
    const doneRepository = {
      update: stubWithArgs(['DONE_ID', 'USER_ID', {DONE_DATA: '..'}], Promise.resolve(fakeDoneItem()))
    };
    ServiceLocator.load({createDoneRepository: () => doneRepository});
    const command = new UpdateDoneCommand();

    const params = {
      doneId: 'DONE_ID',
      userId: 'USER_ID',
      data: {DONE_DATA: '..'}
    };
    return command.execute(params).then(() => {
      expect(doneRepository.update).to.have.been.calledWith('DONE_ID', 'USER_ID', {DONE_DATA: '..'});
    });
  });

  it('returns updated done item with all internal data stripped', () => {
    ServiceLocator.load({
      createDoneRepository: () => ({
        update: () => Promise.resolve(fakeDoneItem())
      })
    });
    const command = new UpdateDoneCommand();

    const params = {};
    return command.execute(params).then(result => {
      expect(result).to.eql({
        doneThing: 'DONE_THING',
        date: 'DATE',
        id: 'DONE_ID',
        userId: 'USER_ID'
      });
    });
  });

  function fakeDoneItem() {
    return {
      doneThing: 'DONE_THING',
      date: 'DATE',
      id: 'DONE_ID',
      userId: 'USER_ID',
      INTERNAL_DATA: '..'
    };
  }

});
