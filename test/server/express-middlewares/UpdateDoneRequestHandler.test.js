
const UpdateDoneRequestHandler = require('../../../src/server/express-middlewares/UpdateDoneRequestHandler');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server UpdateDoneRequestHandler', () => {

  it('updates a done item', () => {
    const doneFormatter = {format: sinon.stub().returns('FORMATTED_DONE')};
    ServiceLocator.load({
      getDoneRepository: () => ({
        update: stubWithArgs(['DONE_ID', 'USER_ID', {DONE_DATA: '..'}], Promise.resolve('UPDATED_DONE'))
      }),
      getDoneFormatter: () => doneFormatter
    });
    const middleware = new UpdateDoneRequestHandler();

    const req = {
      session: {userId: 'USER_ID'},
      params: {id: 'DONE_ID'},
      body: {DONE_DATA: '..'}
    };
    return promisifyExpressMiddleware(middleware, req).then(({res}) => {
      expect(res.setHeader).to.have.been.calledWith('Content-Type', 'application/json');
      expect(res.setHeader).to.have.been.calledWith('Cache-Control', 'no-cache');
      expect(res.send).to.have.been.calledWith('FORMATTED_DONE');
      expect(doneFormatter.format).to.have.been.calledWith('UPDATED_DONE');
    });
  });

});

