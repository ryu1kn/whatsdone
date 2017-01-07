
const DeleteDoneRequestHandler = require('../../../src/server/express-middlewares/DeleteDoneRequestHandler');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server DeleteDoneRequestHandler', () => {

  it('deletes a done item', () => {
    ServiceLocator.load({
      getDoneRepository: () => ({
        remove: stubWithArgs(['DONE_ID', 'USER_ID'], Promise.resolve())
      })
    });
    const middleware = new DeleteDoneRequestHandler();

    const req = {
      session: {userId: 'USER_ID'},
      params: {id: 'DONE_ID'}
    };
    return promisifyExpressMiddleware(middleware, req).then(({res}) => {
      expect(res.end).to.have.been.calledWith();
    });
  });

  it('propagates error', () => {
    ServiceLocator.load({
      getDoneRepository: () => ({
        remove: () => Promise.reject(new Error('UNEXPECTED_ERROR'))
      })
    });
    const middleware = new DeleteDoneRequestHandler();

    const req = {
      params: {},
      session: {}
    };
    return promisifyExpressMiddleware(middleware, req).then(({next}) => {
      expect(next.args[0][0]).to.have.property('message', 'UNEXPECTED_ERROR');
    });
  });

});

