
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

});

