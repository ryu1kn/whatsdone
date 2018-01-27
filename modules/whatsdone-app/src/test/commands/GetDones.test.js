
const GetDonesCommand = require('../../lib/commands/GetDones');
const ServiceLocator = require('../../lib/ServiceLocator');

describe('Server GetDonesCommand', () => {

  it('returns list of dones with the names of their owners', () => {
    const userNameService = {getUsernames: stubWithArgs([['USER_ID']], Promise.resolve([{id: 'USER_ID', name: 'USER'}]))};
    const doneRepository = {read: () => Promise.resolve({items: [{userId: 'USER_ID', SOME_DATA: '..'}]})};
    ServiceLocator.load({
      createUserNameService: () => userNameService,
      createDoneRepository: () => doneRepository
    });
    const command = new GetDonesCommand();

    return command.execute().then(result => {
      expect(result.items).to.eql([{
        userId: 'USER_ID',
        username: 'USER',
        SOME_DATA: '..'
      }]);
    });
  });

});
