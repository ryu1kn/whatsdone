
const ErrorHandler = require('../../../src/server/express-middlewares/ErrorHandler');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server ErrorHandler', () => {

  it('shows NOT FOUND page if error indicates so', () => {
    const logger = {error: sinon.spy()};
    ServiceLocator.load({
      createLogger: () => logger
    });
    const middleware = new ErrorHandler();

    const req = {};
    const err = new Error('[NotFound]: NOT_FOUND');
    return promisifyExpressMiddleware(middleware, req, err).then(result => {
      expect(result.res.status).to.have.been.calledWith(404);
      expect(result.res.render).to.have.been.calledWith('error', {message: '404: Not Found'});
      expect(logger.error.args[0][0]).to.have.string('Error: [NotFound]: NOT_FOUND');
    });
  });

  it('shows an error page with the information that access was denied', () => {
    const logger = {error: sinon.spy()};
    ServiceLocator.load({
      createLogger: () => logger
    });
    const middleware = new ErrorHandler();

    const req = {};
    const err = new Error('[AccessDenied]: ACCESS_DENIED');
    return promisifyExpressMiddleware(middleware, req, err).then(result => {
      expect(result.res.status).to.have.been.calledWith(403);
      expect(result.res.render).to.have.been.calledWith('error', {message: '403: Forbidden'});
      expect(logger.error.args[0][0]).to.have.string('Error: [AccessDenied]: ACCESS_DENIED');
    });
  });

  it('shows a generic error page if an uncategorised error occurred', () => {
    const logger = {error: sinon.spy()};
    ServiceLocator.load({
      createLogger: () => logger
    });
    const middleware = new ErrorHandler();

    const req = {};
    const err = new Error('UNKNOWN ERROR');
    return promisifyExpressMiddleware(middleware, req, err).then(result => {
      expect(result.res.status).to.have.been.calledWith(500);
      expect(result.res.render).to.have.been.calledWith('error', {message: '500: Internal Server Error'});
      expect(logger.error.args[0][0]).to.have.string('Error: UNKNOWN');
    });
  });

});
