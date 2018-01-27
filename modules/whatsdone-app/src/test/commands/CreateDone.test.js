
const CreateDoneCommand = require('../../lib/commands/CreateDone');
const ServiceLocator = require('../../lib/ServiceLocator');

describe('Server CreateDoneCommand', () => {

  it('returns list of dones with the names of their owners', () => {
    const doneRepository = {
      write: stubWithArgs(
        [{userId: 'USER_ID', SOME_DATA: '..'}],
        Promise.resolve({userId: 'USER_ID', SOME_DATA: '..'})
      )
    };
    ServiceLocator.load({
      createDoneRepository: () => doneRepository
    });
    const command = new CreateDoneCommand();

    const params = {
      userId: 'USER_ID',
      username: 'USER',
      data: {SOME_DATA: '..'}
    };
    return command.execute(params).then(result => {
      expect(result).to.eql({
        userId: 'USER_ID',
        username: 'USER',
        SOME_DATA: '..'
      });
    });
  });

});

