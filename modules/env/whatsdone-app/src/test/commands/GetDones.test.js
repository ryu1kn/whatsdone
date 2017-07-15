
const GetDonesCommand = require('../../lib/commands/GetDones');
const ServiceLocator = require('../../lib/ServiceLocator');

describe('Server GetDonesCommand', () => {

  it('returns list of dones with the names of their owners', () => {
    const userRepository = {getByIds: stubWithArgs([['USER_ID']], Promise.resolve([{id: 'USER_ID', name: 'USER'}]))};
    const doneRepository = {read: () => Promise.resolve([{userId: 'USER_ID', SOME_DATA: '..'}])};
    ServiceLocator.load({
      createUserRepository: () => userRepository,
      createDoneRepository: () => doneRepository
    });
    const command = new GetDonesCommand();

    return command.execute().then(result => {
      expect(result).to.eql([{
        userId: 'USER_ID',
        username: 'USER',
        SOME_DATA: '..'
      }]);
    });
  });

});
