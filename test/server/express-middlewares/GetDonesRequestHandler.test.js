
const GetDonesRequestHandler = require('../../../src/server/express-middlewares/GetDonesRequestHandler');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server GetDonesRequestHandler', () => {

  it('returns list of dones with the names of their owners', () => {
    const userRepository = {
      getByIds: stubWithArgs([['USER_ID']], Promise.resolve([{id: 'USER_ID', name: 'USER'}]))
    };
    const doneRepository = {
      read: () => Promise.resolve([{userId: 'USER_ID'}])
    };
    ServiceLocator.load({
      createUserRepository: () => userRepository,
      createDoneRepository: () => doneRepository
    });
    const middleware = new GetDonesRequestHandler();

    const req = {};
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.setHeader).to.have.been.calledWith('Content-Type', 'application/json');
      expect(result.res.send).to.have.been.calledWith([{userId: 'USER_ID', username: 'USER'}]);
    });
  });

  it('propagates error', () => {
    ServiceLocator.load({
      createUserRepository: () => {},
      createDoneRepository: () => ({
        read: () => Promise.reject(new Error('UNEXPECTED_ERROR'))
      })
    });
    const middleware = new GetDonesRequestHandler();

    const req = {};
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.next.args[0][0]).to.have.property('message', 'UNEXPECTED_ERROR');
    });
  });

});

