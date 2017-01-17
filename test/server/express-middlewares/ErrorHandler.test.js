
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
    return promisifyExpressMiddleware(middleware, req, err).then(({res}) => {
      res.status(404);
      res.render('error', {message: ''});
      expect(logger.error.args[0][0]).to.have.string('Error: [NotFound]: NOT_FOUND');
    });

  });

});
