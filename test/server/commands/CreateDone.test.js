
const CreateDoneCommand = require('../../../src/server/commands/CreateDone');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server CreateDoneCommand', () => {

  it('returns list of dones with the names of their owners', () => {
    const userRepository = {
      getById: stubWithArgs(['USER_ID'], Promise.resolve({id: 'USER_ID', name: 'USER'}))
    };
    const doneRepository = {
      write: stubWithArgs(
        [{userId: 'USER_ID', SOME_DATA: '..'}],
        Promise.resolve({userId: 'USER_ID', SOME_DATA: '..'})
      )
    };
    ServiceLocator.load({
      createUserRepository: () => userRepository,
      createDoneRepository: () => doneRepository
    });
    const command = new CreateDoneCommand();

    const params = {
      userId: 'USER_ID',
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

