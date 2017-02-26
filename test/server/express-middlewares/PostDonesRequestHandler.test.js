
const PostDonesRequestHandler = require('../../../src/server/express-middlewares/PostDonesRequestHandler');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server PostDonesRequestHandler', () => {

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
    const middleware = new PostDonesRequestHandler();

    const req = {
      session: {userId: 'USER_ID'},
      body: {SOME_DATA: '..'}
    };
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.setHeader).to.have.been.calledWith('Content-Type', 'application/json');
      expect(result.res.send).to.have.been.calledWith('{"userId":"USER_ID","SOME_DATA":"..","username":"USER"}');
    });
  });

});

